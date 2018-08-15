;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(cl:defpackage #:ironclad-system
  (:use :cl :asdf))

(cl:in-package #:ironclad-system)

;;; easy-to-type readmacro for creating s-boxes and the like

(defun array-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((array-data (read stream nil stream nil))
        (array-element-type `(unsigned-byte ,arg)))
    ;; FIXME: need to make this work for multi-dimensional arrays
    `(make-array ,(length array-data) :element-type ',array-element-type
                :initial-contents ',array-data)))

(defparameter *ironclad-readtable*
  (let ((readtable (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\@ #'array-reader readtable)
    readtable))

(defclass ironclad-source-file (cl-source-file) ())

(defsystem "ironclad"
  :version "0.40"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Guillaume LE VAILLANT <glv@posteo.net>"
  :description "A cryptographic toolkit written in pure Common Lisp"
  :license "BSD 3-Clause"
  :default-component-class ironclad-source-file
  :depends-on (#+sbcl "sb-rotate-byte" #+sbcl "sb-posix" "nibbles")
  :in-order-to ((test-op (test-op "ironclad/tests")))
  :components ((:static-file "LICENSE")
               (:static-file "NEWS")
               (:static-file "README.org")
               (:static-file "TODO")
               (:module "doc"
                :components ((:html-file "ironclad")))
               (:module "src"
                :components ((:file "common" :depends-on ("package" "ccl-opt"))
                             (:file "conditions" :depends-on ("package"))
                             (:file "macro-utils" :depends-on ("package"))
                             (:file "math" :depends-on ("package" "prng"))
                             ;; FIXME: make this depend on :FEATURE :IRONCLAD-GRAY-STREAMS
                             #+(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
                             (:file "octet-stream" :depends-on ("ciphers" "common" "conditions" "digests" "macs" "package"))
                             (:file "package")
                             (:file "padding" :depends-on ("common" "package"))
                             (:file "util" :depends-on ("conditions" "package"))
                             (:module "ccl-opt"
                              :depends-on ("package")
                              :components ((:file "x86oid-vm")))
                             (:module "ciphers"
                              :depends-on ("common" "conditions" "macro-utils" "package" "sbcl-opt")
                              :components ((:file "aes" :depends-on ("cipher"))
                                           (:file "arcfour" :depends-on ("cipher"))
                                           (:file "aria" :depends-on ("cipher"))
                                           (:file "blowfish" :depends-on ("cipher"))
                                           (:file "camellia" :depends-on ("cipher"))
                                           (:file "cast5" :depends-on ("cipher"))
                                           (:file "chacha" :depends-on ("cipher"))
                                           (:file "cipher")
                                           (:file "des" :depends-on ("cipher"))
                                           (:file "idea" :depends-on ("cipher"))
                                           (:file "keystream" :depends-on ("cipher" "chacha" "modes" "salsa20"))
                                           (:file "make-cipher" :depends-on ("cipher"))
                                           (:file "misty1" :depends-on ("cipher"))
                                           (:file "modes" :depends-on ("cipher"))
                                           (:file "rc2" :depends-on ("cipher"))
                                           (:file "rc5" :depends-on ("cipher"))
                                           (:file "rc6" :depends-on ("cipher"))
                                           (:file "salsa20" :depends-on ("cipher"))
                                           (:file "seed" :depends-on ("cipher"))
                                           (:file "serpent" :depends-on ("cipher"))
                                           (:file "sosemanuk" :depends-on ("cipher"))
                                           (:file "square" :depends-on ("cipher"))
                                           (:file "tea" :depends-on ("cipher"))
                                           (:file "threefish" :depends-on ("cipher"))
                                           (:file "twofish" :depends-on ("cipher"))
                                           (:file "xchacha" :depends-on ("cipher" "chacha"))
                                           (:file "xor" :depends-on ("cipher"))
                                           (:file "xsalsa20" :depends-on ("cipher" "salsa20"))
                                           (:file "xtea" :depends-on ("cipher"))))
                             (:module "digests"
                              :depends-on ("ciphers" "common" "conditions" "macro-utils" "package" "sbcl-opt")
                              :components ((:file "adler32" :depends-on ("digest"))
                                           (:file "blake2" :depends-on ("digest"))
                                           (:file "blake2s" :depends-on ("digest"))
                                           (:file "crc24" :depends-on ("digest"))
                                           (:file "crc32" :depends-on ("digest"))
                                           (:file "digest")
                                           (:file "groestl" :depends-on ("digest"))
                                           (:file "jh" :depends-on ("digest"))
                                           (:file "md2" :depends-on ("digest"))
                                           (:file "md4" :depends-on ("digest"))
                                           (:file "md5" :depends-on ("digest"))
                                           (:file "md5-lispworks-int32" :depends-on ("digest"))
                                           (:file "ripemd-128" :depends-on ("digest"))
                                           (:file "ripemd-160" :depends-on ("digest"))
                                           (:file "sha1" :depends-on ("digest"))
                                           (:file "sha256" :depends-on ("digest"))
                                           (:file "sha3" :depends-on ("digest"))
                                           (:file "sha512" :depends-on ("digest"))
                                           (:file "skein" :depends-on ("digest"))
                                           (:file "tiger" :depends-on ("digest"))
                                           (:file "tree-hash" :depends-on ("digest"))
                                           (:file "whirlpool" :depends-on ("digest"))))
                             (:module "kdf"
                              :depends-on ("ciphers" "common" "conditions" "digests" "macs" "package" "prng")
                              :components ((:file "argon2i" :depends-on ("kdf-common"))
                                           (:file "kdf-common")
                                           (:file "password-hash" :depends-on ("pkcs5"))
                                           (:file "pkcs5" :depends-on ("kdf-common"))
                                           (:file "scrypt" :depends-on ("kdf-common" "pkcs5"))))
                             (:module "macs"
                              :depends-on ("common" "conditions" "digests" "package")
                              :components ((:file "blake2-mac" :depends-on ("mac"))
                                           (:file "blake2s-mac" :depends-on ("mac"))
                                           (:file "cmac" :depends-on ("mac"))
                                           (:file "hmac" :depends-on ("mac"))
                                           (:file "mac")
                                           (:file "poly1305" :depends-on ("mac"))
                                           (:file "skein-mac" :depends-on ("mac"))))
                             (:module "prng"
                              :depends-on ("ciphers" "conditions" "digests" "package")
                              :components ((:file "fortuna" :depends-on ("prng" "generator"))
                                           (:file "generator" :depends-on ("prng"))
                                           (:file "os-prng" :depends-on ("prng"))
                                           (:file "prng")))
                             (:module "public-key"
                              :depends-on ("conditions" "digests" "math" "package" "prng")
                              :components ((:file "curve25519" :depends-on ("public-key"))
                                           (:file "curve448" :depends-on ("public-key"))
                                           (:file "dsa" :depends-on ("public-key"))
                                           (:file "ed25519" :depends-on ("public-key"))
                                           (:file "ed448" :depends-on ("public-key"))
                                           (:file "elgamal" :depends-on ("pkcs1" "public-key"))
                                           (:file "pkcs1" :depends-on ("public-key"))
                                           (:file "public-key")
                                           (:file "rsa" :depends-on ("pkcs1" "public-key"))))
                             (:module "sbcl-opt"
                              :depends-on ("common" "package")
                              :components ((:file "fndb")
                                           (:file "x86oid-vm" :depends-on ("fndb"))))))))

(defun ironclad-implementation-features ()
  #+sbcl
  (list* sb-c:*backend-byte-order*
         (if (= sb-vm:n-word-bits 32)
             :32-bit
             :64-bit)
         :ironclad-fast-mod32-arithmetic
         :ironclad-gray-streams
         (when (member :x86-64 *features*)
           '(:ironclad-fast-mod64-arithmetic)))
  #+cmu
  (list (c:backend-byte-order c:*target-backend*)
        (if (= vm:word-bits 32)
            :32-bit
            :64-bit)
        :ironclad-fast-mod32-arithmetic
        :ironclad-gray-streams)
  #+allegro
  (list :ironclad-gray-streams)
  #+lispworks
  (list :ironclad-gray-streams
        ;; Disable due to problem reports from Lispworks users and
        ;; non-obviousness of the fix.
        #+nil
        (when (not (member :lispworks4 *features*))
          '(:ironclad-md5-lispworks-int32)))
  #+openmcl
  (list* :ironclad-gray-streams
         (when (member :x86-64 *features*)
           '(:ironclad-fast-mod64-arithmetic)))
  #+abcl
  (list :ironclad-gray-streams)
  #+ecl
  (list :ironclad-gray-streams)
  #+clisp
  (list :ironclad-gray-streams)
  #-(or sbcl cmu allegro lispworks openmcl abcl ecl clisp)
  nil)

(macrolet ((do-silently (&body body)
             `(handler-bind ((style-warning #'muffle-warning)
                             ;; It's about as fast as we can make it,
                             ;; and a number of the notes relate to code
                             ;; that we're running at compile time,
                             ;; which we don't care about the speed of
                             ;; anyway...
                             #+sbcl (sb-ext:compiler-note #'muffle-warning))
                ,@body)))
  (defmethod perform :around ((op compile-op) (c ironclad-source-file))
    (let ((*readtable* *ironclad-readtable*)
          (*print-base* 10)               ; INTERN'ing FORMAT'd symbols
          (*print-case* :upcase)
          #+sbcl (sb-ext:*inline-expansion-limit* (max sb-ext:*inline-expansion-limit* 1000))
          #+cmu (ext:*inline-expansion-limit* (max ext:*inline-expansion-limit* 1000))
          (*features* (append (ironclad-implementation-features) *features*)))
      (do-silently (call-next-method))))

  (defmethod perform :around ((op load-op) (c ironclad-source-file))
    (do-silently (call-next-method))))

(defmethod perform :after ((op load-op) (c (eql (find-system "ironclad"))))
  (provide :ironclad))


;;; testing

(defclass test-vector-file (static-file)
  ((type :initform "testvec")))

(defpackage :ironclad-tests
  (:nicknames :crypto-tests)
  (:use :cl))

(defsystem "ironclad/tests"
  :depends-on ("ironclad" "rt")
  :version "0.40"
  :in-order-to ((test-op (load-op "ironclad/tests")))
  :perform (test-op (o s)
             (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
                 (error "TEST-OP failed for IRONCLAD/TESTS")))
  :components ((:module "testing"
                :components ((:file "testfuns")
                             (:module "test-vectors"
                              :depends-on ("testfuns")
                              :components ((:file "ironclad")
                                           (:file "padding")
                                           ;; ciphers
                                           (:file "ciphers")
                                           (:file "modes")
                                           (:test-vector-file "3des")
                                           (:test-vector-file "aes")
                                           (:test-vector-file "arcfour")
                                           (:test-vector-file "aria")
                                           (:test-vector-file "blowfish")
                                           (:test-vector-file "camellia")
                                           (:test-vector-file "cast5")
                                           (:test-vector-file "cbc")
                                           (:test-vector-file "cfb")
                                           (:test-vector-file "cfb8")
                                           (:test-vector-file "chacha")
                                           (:test-vector-file "chacha-12")
                                           (:test-vector-file "chacha-8")
                                           (:test-vector-file "ctr")
                                           (:test-vector-file "des")
                                           (:test-vector-file "idea")
                                           (:test-vector-file "misty1")
                                           (:test-vector-file "xor")
                                           (:test-vector-file "ofb")
                                           (:test-vector-file "rc2")
                                           (:test-vector-file "rc5")
                                           (:test-vector-file "rc6")
                                           (:test-vector-file "salsa20")
                                           (:test-vector-file "salsa20-12")
                                           (:test-vector-file "salsa20-8")
                                           (:test-vector-file "seed")
                                           (:test-vector-file "serpent")
                                           (:test-vector-file "sosemanuk")
                                           (:test-vector-file "square")
                                           (:test-vector-file "tea")
                                           (:test-vector-file "threefish1024")
                                           (:test-vector-file "threefish256")
                                           (:test-vector-file "threefish512")
                                           (:test-vector-file "twofish")
                                           (:test-vector-file "xchacha")
                                           (:test-vector-file "xchacha-12")
                                           (:test-vector-file "xchacha-8")
                                           (:test-vector-file "xsalsa20")
                                           (:test-vector-file "xsalsa20-12")
                                           (:test-vector-file "xsalsa20-8")
                                           (:test-vector-file "xtea")
                                           ;; digests
                                           (:file "digests")
                                           (:test-vector-file "adler32")
                                           (:test-vector-file "blake2")
                                           (:test-vector-file "blake2-160")
                                           (:test-vector-file "blake2-256")
                                           (:test-vector-file "blake2-384")
                                           (:test-vector-file "blake2s")
                                           (:test-vector-file "blake2s-128")
                                           (:test-vector-file "blake2s-160")
                                           (:test-vector-file "blake2s-224")
                                           (:test-vector-file "crc24")
                                           (:test-vector-file "crc32")
                                           (:test-vector-file "groestl")
                                           (:test-vector-file "groestl-224")
                                           (:test-vector-file "groestl-256")
                                           (:test-vector-file "groestl-384")
                                           (:test-vector-file "jh")
                                           (:test-vector-file "jh-224")
                                           (:test-vector-file "jh-256")
                                           (:test-vector-file "jh-384")
                                           (:test-vector-file "md2")
                                           (:test-vector-file "md4")
                                           (:test-vector-file "md5")
                                           (:test-vector-file "ripemd-128")
                                           (:test-vector-file "ripemd-160")
                                           (:test-vector-file "sha1")
                                           (:test-vector-file "sha224")
                                           (:test-vector-file "sha256")
                                           (:test-vector-file "sha3")
                                           (:test-vector-file "sha3-224")
                                           (:test-vector-file "sha3-256")
                                           (:test-vector-file "sha3-384")
                                           (:test-vector-file "sha384")
                                           (:test-vector-file "sha512")
                                           (:test-vector-file "shake128")
                                           (:test-vector-file "shake256")
                                           (:test-vector-file "skein1024")
                                           (:test-vector-file "skein1024-384")
                                           (:test-vector-file "skein1024-512")
                                           (:test-vector-file "skein256")
                                           (:test-vector-file "skein256-128")
                                           (:test-vector-file "skein256-160")
                                           (:test-vector-file "skein256-224")
                                           (:test-vector-file "skein512")
                                           (:test-vector-file "skein512-128")
                                           (:test-vector-file "skein512-160")
                                           (:test-vector-file "skein512-224")
                                           (:test-vector-file "skein512-256")
                                           (:test-vector-file "skein512-384")
                                           (:test-vector-file "tiger")
                                           (:test-vector-file "tree-hash")
                                           (:test-vector-file "whirlpool")
                                           ;; kdf
                                           (:file "argon2i")
                                           (:file "pkcs5")
                                           (:file "scrypt")
                                           ;; macs
                                           (:file "macs")
                                           (:test-vector-file "blake2-mac")
                                           (:test-vector-file "blake2s-mac")
                                           (:test-vector-file "cmac")
                                           (:test-vector-file "hmac")
                                           (:test-vector-file "poly1305")
                                           (:test-vector-file "skein-mac")
                                           ;; prng
                                           (:file "prng-tests")
                                           (:test-vector-file "prng")
                                           ;; public key
                                           (:file "public-key")
                                           (:test-vector-file "curve25519")
                                           (:test-vector-file "curve448")
                                           (:test-vector-file "dsa")
                                           (:test-vector-file "ed25519")
                                           (:test-vector-file "ed448")
                                           (:test-vector-file "elgamal-dh")
                                           (:test-vector-file "elgamal-enc")
                                           (:test-vector-file "elgamal-sig")
                                           (:test-vector-file "rsa-enc")
                                           (:test-vector-file "rsa-sig")))))))
