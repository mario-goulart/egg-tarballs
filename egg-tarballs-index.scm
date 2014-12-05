(module make-egg-index ()

(import chicken scheme)
(use data-structures extras files ports posix setup-api srfi-1 srfi-13)

(include "egg-tarballs-version.scm")

(define index-format-version "1")

(define (warn fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append "WARNING: " fmt "\n")
                             args))))

(define (die fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append fmt "\n")
                             args)))
  (exit 1))

(define (egg-versions-dir egg-dir)
  (let ((tags-dir (make-pathname egg-dir "tags")))
    ;; henrietta-cache 1.0.0 changed the cache format.  Here we try to
    ;; cope with both cache formats (with or without the "tags"
    ;; directory)
    (if (directory-exists? tags-dir)
        tags-dir
        egg-dir)))

(define (read-meta-file egg egg-dir)
  (let ((meta-file (make-pathname egg-dir egg "meta")))
    (and (file-read-access? meta-file)
         (handle-exceptions exn
           #f
           (with-input-from-file meta-file read)))))

(define (get-egg-dependencies meta-data)
  ;; Returns (values <build depends> <test depends>)
  (define (deps key)
    (or (and-let* ((d (assq key meta-data)))
          (cdr d))
        '()))
  (values (append (deps 'depends)
                  (deps 'needs))
          (deps 'test-depends)))

(define (tarball-size/sum egg-name egg-version tarball-dir)
  (let* ((egg+version (string-append egg-name "-" egg-version))
         (tarball (make-pathname tarball-dir egg+version "tar.gz"))
         (sum-file (make-pathname tarball-dir egg+version "tar.gz.sha1"))
         (tarball-ok? (file-read-access? tarball))
         (sum-ok? (file-read-access? sum-file)))
    (cond ((and tarball-ok? sum-ok?)
           (values (file-size tarball)
                   (with-input-from-file sum-file read-line)))
          (else
           (unless tarball-ok?
             (warn "could not read ~a" tarball))
           (unless sum-ok?
             (warn "could not read ~a" sum-file))
           (values #f #f)))))

(define (describe-egg egg-name egg-version-dir tarball-dir)
  (let ((meta-data (read-meta-file egg-name egg-version-dir)))
    (if meta-data
        (let ((egg-version (pathname-strip-directory egg-version-dir)))
          (let-values (((build-deps test-deps) (get-egg-dependencies meta-data))
                       ((tarball-size tarball-sum)
                        (tarball-size/sum egg-name egg-version tarball-dir)))
            (when (and tarball-size tarball-sum)
              (write (list (string->symbol egg-name)
                           egg-version
                           tarball-size
                           tarball-sum
                           build-deps
                           test-deps))
              (newline))))
        (warn "could not read meta data for egg ~a (~a)"
              egg-name
              egg-version-dir))))

(define (make-egg-index henrietta-cache-dir tarballs-dir out-dir latest-only?)
  (create-directory out-dir 'recursively)
  (let ((out-file (make-pathname out-dir
                                 (if latest-only?
                                     "index-latest.scm"
                                     "index.scm"))))
    (with-output-to-file out-file
      (lambda ()
        (write index-format-version)
        (newline)
        (for-each
         (lambda (egg-dir)
           (let* ((egg-name (pathname-strip-directory egg-dir))
                  (versions (glob (make-pathname (egg-versions-dir egg-dir) "*")))
                  (tarball-dir (make-pathname tarballs-dir egg-name)))
             (if latest-only?
                 (let ((latest (and (not (null? versions))
                                    (car (sort versions version>=?)))))
                   (and latest
                        (describe-egg egg-name latest tarball-dir)))
                 (for-each (lambda (egg-version-dir)
                             (describe-egg egg-name egg-version-dir tarball-dir))
                           versions))))
         (glob (make-pathname henrietta-cache-dir "*")))))))

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (fprintf port "Usage: ~a [-version] [-O|-out-dir <out dir>] <henrietta cache dir> <tarballs dir>

If <out dir> is not provided, files are written to the current directory.

This program must be run _after_ egg-tarballs, since it needs
information from tarballs and sum files.
"
             (pathname-strip-directory (program-name))))
  (when exit-code (exit exit-code)))

(let ((henrietta-cache-dir #f)
      (tarballs-dir #f)
      (out-dir #f))
  (let loop ((args (command-line-arguments)))
    (cond ((and (null? args)
                (or (not henrietta-cache-dir)
                    (not tarballs-dir)))
           (usage 1))
          ((null? args)
           (let ((out-dir (or out-dir ".")))
             (make-egg-index henrietta-cache-dir tarballs-dir out-dir #f)
             (make-egg-index henrietta-cache-dir tarballs-dir out-dir #t)))
          (else
           (let ((arg (car args)))
             (cond ((or (string=? arg "-h")
                        (string=? arg "-help")
                        (string=? arg "--help"))
                    (usage 0))
                   ((string=? arg "-version")
                    (print egg-tarballs-version)
                    (exit 0))
                   ((or (string=? arg "-O")
                        (string=? arg "-out-dir"))
                    (if (null? (cdr args))
                        (die "-out-dir (-O) requires an argument.")
                        (set! out-dir (cadr args)))
                    (loop (cddr args)))
                   ((and (not (string-prefix? "-" arg))
                         (not henrietta-cache-dir))
                    (set! henrietta-cache-dir arg)
                    (loop (cdr args)))
                   ((and (not (string-prefix? "-" arg))
                         henrietta-cache-dir
                         (not tarballs-dir))
                    (set! tarballs-dir arg)
                    (loop (cdr args)))
                   (else
                    (usage 1))))))))

) ;; end module
