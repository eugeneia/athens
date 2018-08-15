;;;; List of know extensions.

(in-package :file-types)


;;; File type list

(defparameter *file-type-list*
  '(;; Text files
    (("txt")
     :tags (:text)
     :mime ("text" "plain"))
    (("log")
     :tags (:text :log)
     :mime ("text" "plain"))
    (("README")
     :tags (:text)
     :mime ("text" "plain"))
    ;; Public keys
    (("pub")
     :tags (:text :public-key)
     :mime ("application" "pgp-keys"))
    ;; Mail
    (("mail")
     :tags (:text :rfc822-message)
     :mime ("message" "rfc822"))
    (("draft")
     :tags (:text :mail-client-draft)
     :mime ("text" "plain"))
    ;; Markup
    (("mk2")
     :tags (:text :geneva-mk2)
     :mime ("text" "plain"))
    (("meta")
     :tags (:text :meta)
     :mime ("text" "plain"))
    (("html" "htm")
     :tags (:text :hyper-text-markup-language)
     :mime ("text" "html"))
    (("css")
     :tags (:text :cascading-style-sheet)
     :mime ("text" "css"))
    (("tex")
     :tags (:text :tex)
     :mime ("application" "x-tex"))
    (("man" "1" "2" "3" "4" "5" "6" "7" "8" "9")
     :tags (:text :troff :manual-page)
     :mime ("application" "x-troff-man"))
    (("md")
     :tags (:text :markdown)
     :mime ("text" "plain"))
    ;; Source code
    (("c" "h")
     :tags (:text :c)
     :mime ("text" "plain"))
    (("lisp" "asd")
     :tags (:text :common-lisp)
     :mime ("text" "plain"))
    (("asd")
     :tags (:asdf-system-definition))
    (("el")
     :tags (:text :emacs-lisp)
     :mime ("text" "plain"))
    (("rb")
     :tags (:text :ruby)
     :mime ("text" "plain"))
    (("sh" "bash" "csh")
     :tags (:text :shell-script)
     :mime ("application" "x-sh"))
    (("py")
     :tags (:text :python)
     :mime ("text" "plain"))
    (("js" "json")
     :tags (:text :javascript)
     :mime ("text" "javascript"))
    (("mk")
     :tags (:text :make-file)
     :mime ("text" "plain"))
    ;; Resource files
    (("xml")
     :tags (:text :extensible-markup-language)
     :mime ("text" "xml"))
    (("json")
     :tags (:javascript-object-notation))
    ;; Binary files
    ;; Documents
    (("pdf")
     :tags (:binary :portable-document-format)
     :mime ("application" "pdf"))
    (("doc" "dot")
     :tags (:binary :ms-word)
     :mime ("application" "msword"))
    (("ps" "ai" "eps")
     :tags (:binary :postscript))
    ;; Archives
    (("tar")
     :tags (:binary :archive :tarball)
     :mime ("application" "x-tar"))
    (("zip")
     :tags (:binary :archive :zip)
     :mime ("application" "zip"))
    (("rar")
     :tags (:binary :archive :rar)
     :mime ("application" "x-rar-compressed"))
    ;; Compressed
    (("gz")
     :tags (:binary :compressed :gzip)
     :mime ("application" "gzip"))
    (("bz2")
     :tags (:binary :compressed :bzip2)
     :mime ("application" "bzip2"))
    (("lzma" "xz")
     :tags (:binary :compressed :lzma)
     :mime ("application" "x-lzma"))
    ;; Images
    (("png")
     :tags (:binary :image :portable-network-graphics)
     :mime ("image" "png"))
    (("gif")
     :tags (:binary :image :graphics-interchange-format)
     :mime ("image" "gif"))
    (("jpg" "jpeg")
     :tags (:binary :image :jpeg)
     :mime ("image" "jpeg"))
    (("svg")
     :tags (:binary :image :scalable-vector-graphics)
     :mime ("image" "svg+xml"))
    (("xcf")
     :tags (:binary :image :gimp))
    ;; Audio & Video
    (("ogg" "oga" "ogv")
     :tags (:binary :ogg)
     :mime ("application" "ogg"))
    (("ogg")
     :tags (:audio :video))
    (("oga")
     :tags (:audio)
     :mime ("audio" "ogg"))
    (("ogv")
     :tags (:video)
     :mime ("video" "ogg"))
    (("flac")
     :tags (:binary :audio :flac)
     :mime ("audio" "flac"))
    (("wav")
     :tags (:binary :audio :wave)
     :mime ("audio" "x-wav"))
    (("aif" "aiff" "aifc")
     :tags (:binary :audio :audio-interchange)
     :mime ("audio" "x-aiff"))
    (("mpga" "mpega" "mp2" "mp3" "m4a")
     :tags (:binary :audio :mpeg)
     :mime ("audio" "mpeg"))
    (("mp3")
     :tags (:mpeg-3))
    (("mka" "mkv" "mpv")
     :tags (:binary :matroska))
    (("mka")
     :tags (:audio)
     :mime ("audio" "x-matroska"))
    (("mkv" "mpv")
     :tags (:video)
     :mime ("video" "x-matroska"))
    (("avi")
     :tags (:binary :video :audio-video-interleave)
     :mime ("video" "x-msvideo"))
    (("mpeg" "mpg" "mpe" "mp4")
     :tags (:binary :video :mpeg)
     :mime ("video" "mpeg"))
    (("mp4")
     :tags (:mpeg-4)
     :mime ("video" "mp4"))
    (("webm")
     :tags (:binary :video :webm)
     :mime ("video" "webm")))
  "List of known file extensions and associated tags.")
