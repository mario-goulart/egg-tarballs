(module egg-tarballs ()

(import chicken scheme)
(use files posix data-structures utils srfi-1 srfi-13 simple-sha1 extras)

(include "egg-tarballs-version.scm")

(define tar "tar")
(define tar-options "cf")
(define gzip "gzip")
(define gzip-options "-9")
(define cp "cp")
(define cp-options "-R")
(define rm "rm")
(define rm-options "-rf")

(define *verbose* #f)

(define (concat args)
  (string-intersperse (map ->string args) " "))

(define-syntax run
  (syntax-rules ()
    ((_ . exprs)
     (let ((cmd (concat `exprs)))
       (when *verbose*
         (print cmd))
       (system* cmd)))))

(define (create-egg-tarball egg-name egg-version-dir tarballs-dir)
  (let* ((egg-tarball-dir (make-pathname tarballs-dir egg-name))
         (egg-version-tarball-dir
          (sprintf "~a-~a"
                   egg-name
                   (pathname-strip-directory egg-version-dir)))
         (tar-file (make-pathname #f egg-version-tarball-dir "tar"))
         (gzip-file (make-pathname #f tar-file "gz"))
         (sum-file (make-pathname (list tarballs-dir egg-name) gzip-file "sha1")))
    (if (file-exists? sum-file)
        (printf "Skipping ~a (sha1sum exists)\n" egg-version-tarball-dir)
        (begin
          (printf "Creating tarball for ~a\n" egg-version-tarball-dir)
          (create-directory  egg-tarball-dir 'with-parents)
          (run ,cp ,cp-options
               ,egg-version-dir
               ,(make-pathname egg-tarball-dir egg-version-tarball-dir))
          (change-directory egg-tarball-dir)
          (run ,tar ,tar-options ,tar-file ,egg-version-tarball-dir)
          (run ,gzip ,gzip-options ,tar-file)
          (with-output-to-file sum-file
            (lambda ()
              (print (sha1sum gzip-file))))
          (run ,rm ,rm-options ,tar-file ,egg-version-tarball-dir)))))

(define (egg-versions-dir egg-dir)
  (let ((tags-dir (make-pathname egg-dir "tags")))
    ;; henrietta-cache 1.0.0 changed the cache format.  Here we try to
    ;; cope with both cache formats (with or without the "tags"
    ;; directory)
    (if (directory-exists? tags-dir)
        tags-dir
        egg-dir)))

(define (create-tarballs henrietta-cache-dir tarballs-dir)
  (create-directory tarballs-dir 'with-parents)
  (for-each
   (lambda (egg-dir)
     (let* ((egg-name (pathname-strip-directory egg-dir))
            (versions (glob (make-pathname (egg-versions-dir egg-dir) "*"))))
       (for-each (lambda (egg-version-dir)
                   (create-egg-tarball egg-name egg-version-dir tarballs-dir))
                 versions)))
   (glob (make-pathname henrietta-cache-dir "*"))))

(define (absolute-path path)
  (if (absolute-pathname? path)
      path
      (normalize-pathname (make-pathname (current-directory) path))))

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (fprintf port "Usage: ~a [-verbose] [-version] <henrietta cache dir> <tarballs dir>\n"
             (pathname-strip-directory (program-name))))
  (when exit-code (exit exit-code)))

(let* ((args (command-line-arguments))
       (non-option-args (remove (lambda (arg)
                                  (string-prefix? "-" arg))
                                args)))
  (when (or (member "--help" args)
            (member "-help" args)
            (member "-h" args))
    (usage 0))
  (when (< (length non-option-args) 2)
    (usage 1))
  (when (member "-verbose" args)
    (set! *verbose* #t))
  (when (member "-version" args)
    (print egg-tarballs-version)
    (exit 0))
  (create-tarballs (absolute-path (car non-option-args))
                   (absolute-path (cadr non-option-args))))

) ;; end module
