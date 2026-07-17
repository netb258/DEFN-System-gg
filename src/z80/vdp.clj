(ns z80.vdp
  (:import [com.codingrodent.microprocessor IMemory IBaseDevice]
           [com.codingrodent.microprocessor.Z80 Z80Core]
           [com.codingrodent.microprocessor.Z80 CPUConstants$RegisterNames]))

(defrecord VdpState [
  vram
  cram
  cram-latch          ;; Game Gear specific: Latches the low byte of the 12-bit color
  regs
  first-byte?
  command-byte
  vram-pointer
  operation
  read-buffer
  current-scan-line
  vblank-active?
  sprite-collision?])

(defn create-vdp []
  (map->VdpState {
    :vram (byte-array 16384)   ;; 16KB of VRAM.
    ;; NOTE: Added for the GG version. 64 bytes of CRAM instead of 32.
    :cram (int-array 64)       ;; Game Gear: 64 bytes of CRAM (32 colors * 2 bytes each)
    :cram-latch 0              ;; Game Gear: Holds the first byte of a 12-bit color write
    :regs (int-array 16)       ;; 16 registers.
    :first-byte? true          ;; 1-bit flip-flop for control port commands.
    :command-byte 0            ;; Temporary holding buffer for 2-byte commands.
    :vram-pointer 0            ;; VDP Video Memory 14-bit Address Pointer.
    :operation 0               ;; Current operating mode (0, 1, 2, or 3).
    :read-buffer 0             ;; 8-bit VRAM cache.
    :current-scan-line 0       ;; V-COUNTER tracking.
    :vblank-active? false      ;; V-BLANK interrupt active flag.
    :sprite-collision? false})) ;; Sprite collision active flag.

(defn get-v-counter [^VdpState vdp]
  ;; Game Gear uses a dedicated screen/refresh timing model.
  (let [line (int (.current-scan-line vdp))]
    (cond
      (<= line 242) line
      (<= line 312) (+ 0xBA (- line 243))
      :else 0xFF)))

(defn calculate-h-counter [^Z80Core cpu]
  (let [current-cycles (.getTStates cpu)
        line-cycles (mod current-cycles 227)
        h-val (quot (* line-cycles 3) 2)]
    (if (<= h-val 225)
      h-val
      (bit-and (+ 202 (- h-val 226)) 0xFF))))

;; NOTE: Changed data write for the GG version.
(defn data-write! [^VdpState vdp ^long value]
  (let [op (int (.operation vdp))
        loc (int (.vram-pointer vdp))]
    (cond
      ;; --- VRAM Write (Modes 0, 1, 2) ---
      (or (= op 0) (= op 1) (= op 2))
      (let [address (bit-and loc 0x3FFF)
            ^bytes vram (.vram vdp)]
        (aset vram address (unchecked-byte value))
        (-> vdp
            (assoc :vram-pointer (bit-and (inc loc) 0x3FFF))
            (assoc :read-buffer value)
            (assoc :first-byte? true)))

      ;; --- CRAM (Palette) Write (Mode 3) ---
      (= op 3)
      (let [cram-addr (bit-and loc 0x3F) ;; GG has 64 bytes of CRAM (0x00 to 0x3F)
            is-even? (zero? (bit-and cram-addr 0x01))
            ^ints cram (.cram vdp)]
        (if is-even?
          ;; Even byte: Just cache it in the CRAM latch, don't write to CRAM yet
          (-> vdp
              (assoc :cram-latch (bit-and value 0xFF))
              (assoc :vram-pointer (bit-and (inc loc) 0x3FFF))
              (assoc :read-buffer value)
              (assoc :first-byte? true))
          
          ;; Odd byte: Combine latched even byte with current odd byte and commit
          (let [even-byte (:cram-latch vdp)
                odd-byte (bit-and value 0xFF)
                ;; Storing them safely into sequential indices in our int-array
                even-cram-idx (dec cram-addr)
                odd-cram-idx cram-addr]
            (aset cram even-cram-idx (int even-byte))
            (aset cram odd-cram-idx (int odd-byte))
            (-> vdp
                (assoc :vram-pointer (bit-and (inc loc) 0x3FFF))
                (assoc :read-buffer value)
                (assoc :first-byte? true)))))

      :else
      (-> vdp
          (assoc :vram-pointer (bit-and (inc loc) 0x3FFF))
          (assoc :read-buffer value)
          (assoc :first-byte? true)))))


(defn data-read! [^VdpState vdp]
  (let [loc (int (.vram-pointer vdp))
        address (bit-and loc 0x3FFF)
        ^bytes vram-arr (.vram vdp)
        return-val (bit-and (int (.read-buffer vdp)) 0xFF)
        next-buffered-val (bit-and (aget vram-arr address) 0xFF)
        next-loc (bit-and (inc loc) 0x3FFF)]
    [return-val (assoc vdp 
                       :vram-pointer next-loc 
                       :read-buffer next-buffered-val
                       :first-byte? true)]))

(defn control-write! [^VdpState vdp ^long value]
  (if (:first-byte? vdp)
    ;; First byte: Save and wait for the second byte
    (let [clean-val (bit-and value 0xFF)
          old-loc (int (.vram-pointer vdp))
          new-loc (bit-or (bit-and old-loc 0x3F00) clean-val)
          
          ;; Hardware Prefetch: If currently in Read Mode (op 0), update read-buffer instantly!
          op (int (.operation vdp))
          ^bytes vram-arr (.vram vdp)
          updated-buffer (if (= op 0) 
                           (bit-and (aget vram-arr (bit-and new-loc 0x3FFF)) 0xFF) 
                           (int (.read-buffer vdp)))]
      (assoc vdp 
             :command-byte clean-val 
             :vram-pointer new-loc
             :read-buffer updated-buffer
             :first-byte? false))
    
    ;; Second byte received: Combine both to process command
    (let [low-byte (bit-and (:command-byte vdp) 0xFF)
          high-byte (bit-and value 0xFF)
          ;; Extract Operation Code (Top 2 bits of the second byte)
          code-type (bit-shift-right (bit-and high-byte 0xC0) 6)
          ;; Extract Address (Lower 6 bits of high byte + full low byte)
          new-loc (bit-or low-byte (bit-shift-left (bit-and high-byte 0x3F) 8))]
      (cond
        ;; Mode 0: VRAM Read
        (= code-type 0)
        (let [^bytes vram-arr (.vram vdp)
              buffered-val (if (< new-loc (count vram-arr)) (bit-and (aget vram-arr new-loc) 0xFF) 0)]
          (assoc vdp :vram-pointer (inc new-loc) :operation code-type :read-buffer buffered-val :first-byte? true))

        ;; Mode 1: VRAM Write
        (= code-type 1) (assoc vdp :vram-pointer new-loc :operation code-type :first-byte? true)

        ;; Mode 2: VDP Register Write (Top bits are 10xx xxxx)
        (= code-type 2)
        (let [reg-num (bit-and high-byte 0x0F) 
              ^ints regs-arr (.regs vdp)]
          (when (< reg-num (alength regs-arr))
            (aset regs-arr reg-num (int low-byte)))
          (assoc vdp :first-byte? true))

        ;; Mode 3: CRAM Pointer Setup (Top bits are 11xx xxxx)
        ;; The operation must be set to 3 so data-write! knows to route incoming bytes to CRAM.
        (= code-type 3) (assoc vdp :vram-pointer new-loc :operation 3 :first-byte? true)
        :else (assoc vdp :first-byte? true)))))


(defn read-status-port! [^VdpState vdp ^Z80Core cpu]
  (let [vblank-bit (if (:vblank-active? vdp) 0x80 0x00)
        collision-bit (if (:sprite-collision? vdp) 0x20 0x00)
        current-status (bit-or vblank-bit collision-bit)]
    (.setInterrupt cpu false)
    [current-status (assoc vdp 
                           :first-byte? true 
                           :vblank-active? false
                           :sprite-collision? false)]))
