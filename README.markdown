# cl-salesforce-bulk - salesforce bulk api client

## This is

Common Lisp client library for accessing the asynchronous Salesforce.com Bulk API.

## Usage

### create connection

```common-lisp
(defvar *conn* (cl-salesforce-bulk:login "tamura.shingo@gmail.com" "password" "39.0")
```

if you want to login sandbox, set `sandbox-mode` parameter.

```common-lisp
(defvar *conn* (cl-salesforce-bulk:login "tamura.shingo@gmail.com" "password" "39.0" :sandbox-mode t)
```


## Installation

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2019 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the MIT License.
