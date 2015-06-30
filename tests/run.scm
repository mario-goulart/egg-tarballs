(use test setup-api posix files utils simple-sha1)

(define egg-tarballs
  (make-pathname
   (if (get-environment-variable "SALMONELLA_RUNNING")
       #f
       (program-path))
   "egg-tarballs"))

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

(test (sha1sum "tarballs/foo/foo-1.0.tar.gz")
      (with-input-from-file "tarballs/foo/foo-1.0.tar.gz.sha1" read-line))

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

(test (sha1sum "tarballs/foo/foo-1.0.tar.gz")
      (with-input-from-file "tarballs/foo/foo-1.0.tar.gz.sha1" read-line))

(test-end "new cache format")


(test-end "egg-tarballs")

(test-exit)
