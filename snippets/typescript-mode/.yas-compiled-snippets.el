;;; Compiled snippets and support files for `typescript-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'typescript-mode
                     '(("cdkclass" "import cdk = require('@aws-cdk/core');\n\nexport interface ${1:MyFancy}StackProps extends cdk.StackProps { }\n\nexport class $1Stack extends cdk.Stack {\n\n    constructor(scope: cdk.Construct, id: string, props: $1StackProps) {\n        super(scope, id, props);\n        $0\n    }\n}\n" "cdk-class" nil nil nil "/Users/hvr/.emacs.d/snippets/typescript-mode/cdk-class" nil nil)))


;;; Do not edit! File generated at Wed May 20 15:01:51 2020
