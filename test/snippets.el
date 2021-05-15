;;========================================================================
;; test for comparing position in source code
;; given by two lists (linno colno)
;;========================================================================

(abaplib-util-compare-source-code  (list 6 2) (list 4 6));  => nil
(abaplib-util-compare-source-code  (list 4 6) (list 6 6));  => t
(abaplib-util-compare-source-code  (list 4 6) (list 6 4));  => t
(abaplib-util-compare-source-code  (list 6 2) (list 4 1));  => nil
(abaplib-util-compare-source-code  (list 6 6) (list 6 6));  => t

;; sort list of source code positions by first appearance
(-sort 'abaplib-util-compare-source-code  (list (list 6 2) (list 4 6) (list 6 6) (list 6 4) (list 4 1)));
(--sort (abaplib-util-compare-source-code it other)  (list (list 6 2) (list 4 6) (list 6 6) (list 6 4) (list 4 1)));
;; => ((4 1) (4 6) (6 2) (6 4) (6 6))

;;========================================================================
;; append to (end of) list
;;========================================================================

(let ((list1 (list (list 1 2))))
  (-snoc list1 (list 3 4)));

(let ((list1))
  (setq list1 (-snoc list1 (list 1 2)))
  (setq list1 (-snoc list1 (list 3 4)))
  list1);

;;========================================================================
;; ABAP Unit - RESPONSE-MESSAGE_BODY
;;========================================================================

;; <<<1>>> tests passed successfully

[
  "runResult",
  null,
  [
    "program",
    {
      "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt",
      "type": "CLAS/OC",
      "name": "CL_SVER_DBI_NATIVE_SQL_STMT",
      "uriType": "semantic"
    },
    [
      "testClasses",
      null,
      [
        "testClass",
        {
          "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST",
          "name": "LTC_NATIVE_SQL_STMT_TEST",
          "uriType": "semantic",
          "durationCategory": "short",
          "riskLevel": "harmless"
        },
        [
          "testMethods",
          null,
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_FROM_INTO",
              "name": "SELECT_FROM_INTO",
              "executionTime": "0",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_INTO_CORR1",
              "name": "SELECT_INTO_CORR1",
              "executionTime": "0.01",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_INTO_CORR2",
              "name": "SELECT_INTO_CORR2",
              "executionTime": "0.01",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_INTO_CORR_TABLE1",
              "name": "SELECT_INTO_CORR_TABLE1",
              "executionTime": "0.01",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_INTO_CORR_TABLE2",
              "name": "SELECT_INTO_CORR_TABLE2",
              "executionTime": "0.01",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_OLD",
              "name": "SELECT_OLD",
              "executionTime": "0.01",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_OLD_INTO1",
              "name": "SELECT_OLD_INTO1",
              "executionTime": "0",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_OLD_INTO2",
              "name": "SELECT_OLD_INTO2",
              "executionTime": "0",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_SIMPLE",
              "name": "SELECT_SIMPLE",
              "executionTime": "0",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_UPTONROWS1",
              "name": "SELECT_UPTONROWS1",
              "executionTime": "0.01",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_UPTONROWS2",
              "name": "SELECT_UPTONROWS2",
              "executionTime": "0.01",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=SELECT_UPTONROWS3",
              "name": "SELECT_UPTONROWS3",
              "executionTime": "0",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=WITH_CTE1",
              "name": "WITH_CTE1",
              "executionTime": "0.02",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=WITH_CTE2",
              "name": "WITH_CTE2",
              "executionTime": "0.01",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=WITH_CTE3",
              "name": "WITH_CTE3",
              "executionTime": "0.01",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=WITH_CTE4",
              "name": "WITH_CTE4",
              "executionTime": "0",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=WITH_TILDE1",
              "name": "WITH_TILDE1",
              "executionTime": "0.02",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/cl_sver_dbi_native_sql_stmt#testclass=LTC_NATIVE_SQL_STMT_TEST;testmethod=WITH_TILDE2",
              "name": "WITH_TILDE2",
              "executionTime": "0.02",
              "uriType": "semantic",
              "unit": "s"
            }
          ]
        ]
      ]
    ]
  ]
]

;; <<<2>>> not all tests passed successfully

[
  "runResult",
  null,
  [
    "program",
    {
      "uri": "/sap/bc/adt/oo/classes/zsmp_cl_test_cases",
      "type": "CLAS/OC",
      "name": "ZSMP_CL_TEST_CASES",
      "uriType": "semantic"
    },
    [
      "testClasses",
      null,
      [
        "testClass",
        {
          "uri": "/sap/bc/adt/oo/classes/zsmp_cl_test_cases#testclass=LTC_TEST_CASES",
          "name": "LTC_TEST_CASES",
          "uriType": "semantic",
          "durationCategory": "short",
          "riskLevel": "harmless"
        },
        [
          "testMethods",
          null,
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/zsmp_cl_test_cases#testclass=LTC_TEST_CASES;testmethod=M_0001",
              "name": "M_0001",
              "executionTime": "0",
              "uriType": "semantic",
              "unit": "s"
            }
          ],
          [
            "testMethod",
            {
              "uri": "/sap/bc/adt/oo/classes/zsmp_cl_test_cases#testclass=LTC_TEST_CASES;testmethod=M_0002",
              "name": "M_0002",
              "executionTime": "0",
              "uriType": "semantic",
              "unit": "s"
            },
            [
              "alerts",
              null,
              [
                "alert",
                {
                  "kind": "failedAssertion",
                  "severity": "critical"
                },
                [
                  "title",
                  null,
                  "Critical Assertion Error: 'M_0002: ASSERT_EQUALS'"
                ],
                [
                  "details",
                  null,
                  [
                    "detail",
                    {
                      "text": "Different values"
                    },
                    [
                      "details",
                      null,
                      [
                        "detail",
                        {
                          "text": "Expected [0] Actual [1]"
                        }
                      ]
                    ]
                  ],
                  [
                    "detail",
                    {
                      "text": "Test 'LTC_TEST_CASES->M_0002' in Main Program 'ZSMP_CL_TEST_CASES============CP'"
                    }
                  ]
                ],
                [
                  "stack",
                  null,
                  [
                    "stackEntry",
                    {
                      "uri": "/sap/bc/adt/oo/classes/zsmp_cl_test_cases/includes/testclasses#start=19,0",
                      "type": "CLAS/OCN/testclasses",
                      "name": "ZSMP_CL_TEST_CASES",
                      "description": "Include: <ZSMP_CL_TEST_CASES============CCAU> Line: <19> (M_0002)"
                    }
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
]
