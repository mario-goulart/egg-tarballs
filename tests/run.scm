(cond-expand
  (chicken-4
   (use test setup-api posix files utils simple-sha1)
   (define read-list read-file))
  ((or chicken-5 chicken-6)
   (import (chicken file)
           (chicken format)
           (chicken io)
           (chicken pathname)
           (chicken process)
           (chicken process-context)
           (chicken string))
   (import simple-sha1 test))
  (else
   (error "Unsupported CHICKEN version.")))

(define egg-tarballs
  (make-pathname
   (if (get-environment-variable "SALMONELLA_RUNNING")
       #f
       (pathname-directory (car (argv))))
   "egg-tarballs"))

(define egg-tarballs-index
  (make-pathname
   (if (get-environment-variable "SALMONELLA_RUNNING")
       #f
       (pathname-directory (car (argv))))
   "egg-tarballs-index"))

(handle-exceptions exn
  'ignore
  (delete-directory "tarballs" 'recursively))

(system* (sprintf "~a henrietta-cache-old-format tarballs" egg-tarballs))

(test-begin "egg-tarballs")

(test-begin "old cache format")

(test "tarballs/foo/foo-1.0.tar.gz"
      (file-exists? "tarballs/foo/foo-1.0.tar.gz"))

(test "tarballs/foo/foo-1.0.tar.gz.sha1"
      (file-exists? "tarballs/foo/foo-1.0.tar.gz.sha1"))

(test (list (sha1sum "tarballs/foo/foo-1.0.tar.gz") "foo-1.0.tar.gz")
      (string-split
       (with-input-from-file "tarballs/foo/foo-1.0.tar.gz.sha1" read-line)
       "\t"))

(test-end "old cache format")


(handle-exceptions exn
  'ignore
  (delete-directory "tarballs" 'recursively))

(system* (sprintf "~a henrietta-cache-new-format tarballs" egg-tarballs))

(test-begin "new cache format")

(test "tarballs/foo/foo-1.0.tar.gz"
      (file-exists? "tarballs/foo/foo-1.0.tar.gz"))

(test "tarballs/foo/foo-1.0.tar.gz.sha1"
      (file-exists? "tarballs/foo/foo-1.0.tar.gz.sha1"))

(test (list (sha1sum "tarballs/foo/foo-1.0.tar.gz") "foo-1.0.tar.gz")
      (string-split
       (with-input-from-file "tarballs/foo/foo-1.0.tar.gz.sha1" read-line)
       "\t"))

(test-end "new cache format")


(test-end "egg-tarballs")


(test-begin "egg-tarballs-index")

(for-each
 (lambda (chicken-version)
   (test-begin (sprintf "egg-tarballs-index for CHICKEN ~a eggs"
                        chicken-version))
   (handle-exceptions exn 'ignore (delete-file "index.scm"))
   (handle-exceptions exn 'ignore (delete-file "index-latest.scm"))

   (system*
    (sprintf
     "~a -chicken-version ~a -henrietta-cache-dir henrietta-cache-new-format -tarballs-dir tarballs"
                     egg-tarballs-index chicken-version))

   (let ((index-data (with-input-from-file "index.scm" read-list)))
     (test "index format version"
           "2"
           (car index-data))

     (test "egg data"
           '(foo "1.0" (bar) (baz))
           (let ((egg-data (cadr index-data)))
             ;; Ignore size and sum
             (list (list-ref egg-data 0)
                   (list-ref egg-data 1)
                   (list-ref egg-data 4)
                   (list-ref egg-data 5)))))
   (test-end (sprintf "egg-tarballs-index for CHICKEN ~a eggs"
                      chicken-version)))
 '(4 5))

(test-end "egg-tarballs-index")

(test-exit)
