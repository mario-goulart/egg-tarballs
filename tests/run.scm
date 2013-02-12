(use test setup-api posix files utils)

(define egg-tarballs
  (make-pathname (list chicken-prefix "bin") "egg-tarballs"))

(system* "rm -rf tarballs")
(system* (sprintf "~a henrietta-cache tarballs" egg-tarballs))

(test-begin "egg-tarballs")

(test "tarballs/foo/foo-1.0.tar.gz"
      (file-exists? "tarballs/foo/foo-1.0.tar.gz"))

(test "tarballs/foo/foo-1.0.tar.gz.sha1"
      (file-exists? "tarballs/foo/foo-1.0.tar.gz.sha1"))

(test "ced3fafe709d5c715a939cce554a9528eab054f7"
      (with-input-from-file "tarballs/foo/foo-1.0.tar.gz.sha1" read-line))

(test-end "egg-tarballs")