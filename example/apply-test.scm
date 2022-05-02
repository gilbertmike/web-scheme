(save env)
(assign proc (op lookup-variable-value) (const pp) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const apply) (reg env))
(assign val (const (1 2 3)))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const +) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch1238))
compiled-branch1237
(assign continue (label after-call1236))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch1238
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1236
(assign argl (op list) (reg val))
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch1241))
compiled-branch1240
(assign continue (label after-call1239))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch1241
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1239
(restore env)
(save env)
(assign proc (op lookup-variable-value) (const pp) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const apply) (reg env))
(assign val (const (1 2 3)))
(assign argl (op list) (reg val))
(assign val (op make-compiled-procedure) (label entry1229) (reg env))
(goto (label after-lambda1228))
entry1229
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const x1211) (reg argl) (reg env))
(assign val (op lookup-variable-value) (const x1211) (reg env))
(goto (reg continue))
after-lambda1228
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch1232))
compiled-branch1231
(assign continue (label after-call1230))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch1232
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1230
(assign argl (op list) (reg val))
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch1235))
compiled-branch1234
(assign continue (label after-call1233))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch1235
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1233
(restore env)
(save env)
(assign proc (op lookup-variable-value) (const pp) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const apply) (reg env))
(assign val (const ()))
(assign argl (op list) (reg val))
(assign val (op make-compiled-procedure) (label entry1221) (reg env))
(goto (label after-lambda1220))
entry1221
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const x1210) (reg argl) (reg env))
(assign val (op lookup-variable-value) (const x1210) (reg env))
(goto (reg continue))
after-lambda1220
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch1224))
compiled-branch1223
(assign continue (label after-call1222))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch1224
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1222
(assign argl (op list) (reg val))
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch1227))
compiled-branch1226
(assign continue (label after-call1225))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch1227
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1225
(restore env)
(assign proc (op lookup-variable-value) (const pp) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const apply) (reg env))
(assign val (const (1 2 3)))
(assign argl (op list) (reg val))
(assign val (const 2))
(assign argl (op cons) (reg val) (reg argl))
(assign val (const 1))
(assign argl (op cons) (reg val) (reg argl))
(assign val (op make-compiled-procedure) (label entry1213) (reg env))
(goto (label after-lambda1212))
entry1213
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const x1209) (reg argl) (reg env))
(assign val (op lookup-variable-value) (const x1209) (reg env))
(goto (reg continue))
after-lambda1212
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch1216))
compiled-branch1215
(assign continue (label after-call1214))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch1216
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1214
(assign argl (op list) (reg val))
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch1219))
compiled-branch1218
(assign continue (label halt))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch1219
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (label halt))
after-call1217
