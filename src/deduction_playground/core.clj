(ns deduction-playground.core
  (:require [deduction-playground.proof-new :refer [proof step-f step-b choose-option rename-var]]
            [deduction-playground.read-rules :refer [read-rules read-theorems reset-rules reset-theorems export-theorem]]
            [deduction-playground.printer :refer [pprint]]))

(read-rules "resources/rules.clj")
(read-theorems "resources/theorems.clj")
