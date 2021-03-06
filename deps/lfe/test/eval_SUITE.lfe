;; Copyright (c) 2008-2013 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; File    : eval_SUITE.lfe
;; Author  : Robert Virding
;; Purpose : Dynamic eval test suite.

;; The test cases are all dynamically built code which we call eval on.

(include-file "test_server.lfe")

(defmodule eval_SUITE
  (export (all 0) (suite 0) (groups 0) (init_per_suite 1) (end_per_suite 1)
	  (init_per_group 2) (end_per_group 2)
	  ;; (init_per_testcase 2) (end_per_testcase 2)
	  (guard_1 1) (binary_1 1) (binding_1 1)))

(defmacro MODULE () `'eval_SUITE)

(defun all ()
  ;; (: test_lib recompile (MODULE))
  (list 'guard_1 'binary_1 'binding_1))

;;(defun suite () (list (tuple 'ct_hooks (list 'ts_install_cth))))
(defun suite () ())

(defun groups () ())

(defun init_per_suite (config) config)

(defun end_per_suite (config) 'ok)

(defun init_per_group (name config) config)

(defun end_per_group (name config) config)

(defun guard_1
  (['suite] ())
  (['doc] '"Guard tests.")
  ([config] (when (is_list config))
   (line (test-pat 'no (eval `(case 42
				(42 (when (== (+ 'a 4) 4)) 'yes)
				(42 'no)))))
   (line (test-pat 'no (eval `(case 42
				(42 (when (== (+ 6 4) 4)) 'yes)
				(42 'no)))))

   'ok))

(defun binary_1
  (['suite] ())
  (['doc] '"Test binary creation and matching.")
  ([config] (when (is_list config))
   ;; Creating and pulling apart simple binary.
   (let ((f14-64 #b(64 44 0 0 0 0 0 0)))
     (line (test-pat f14-64 (eval `(binary (14 float (size 64))))))
     (line (test-pat f14-64 (eval `(binary (,f14-64 binary)))))
     (line (test-pat f14-64 (eval `(binary (,f14-64 binary (size all))))))
     (line (test-pat (tuple 17 47)
		     (eval `(let (((binary (b1 bits (size 17)) (b2 bits)) ,f14-64))
			      (tuple (bit_size b1) (bit_size b2))))))
     )
   ;; Matching out values to use as size.
   (line (test-pat (tuple 2 #b("AB") #b("CD"))
		   (eval `(let (((binary s (b binary (size s)) (rest binary))
				 #b(2 "AB" "CD")))
			    (tuple s b rest)))))
   (line (test-pat (tuple 2 #b("AB") #b("CD"))
		   (eval '(flet ((a ([(binary s
					      (b binary (size s))
					      (rest binary))]
				     (tuple s b rest))))
			    (a #b(2 "AB" "CD"))))))

   'ok))

(defun binding_1
  (['suite] ())
  (['doc] '"Test function bindings.")
  ([config] (when (is_list config))
    (let (((1 2)
           (funcall (: lfe_eval expr
             '(lambda () (foo 1 2))
             ;; We evaluate the above lambda form in a new environment that
             ;; contains a binding for the function foo/2.
             (: lfe_eval add_expr_func 'foo 2 (lambda (a b) (list a b))
               (: lfe_lib new_env))))))
      'ok)))
