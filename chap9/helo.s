
---- prog ----
:L1
const t5, #int 4
new-instance t4, Ljava/lang/Integer;
invoke-direct {t4, t5}, Ljava/lang/Integer;.<init>:(I)V
check-cast t2, Ljava/lang/Integer;
check-cast t4, Ljava/lang/Integer;
invoke-virtual {t2}, Ljava/lang/Integer;.intValue:()I
move-result t7
invoke-virtual {t4}, Ljava/lang/Integer;.intValue:()I
move-result t8
add-int t7, t7, t8
new-instance t6, Ljava/lang/Integer;
invoke-direct {t6, t7, Ljava/lang/Integer;.<init>:(I)V
move-object v0, t3
move-object v1, t1
const t10, #int 2
new-instance t9, Ljava/lang/Integer;
invoke-direct {t9, t10}, Ljava/lang/Integer;.<init>:(I)V
check-cast t6, Ljava/lang/Integer;
check-cast t9, Ljava/lang/Integer;
invoke-virtual {t6}, Ljava/lang/Integer;.intValue:()I
move-result t12
invoke-virtual {t9}, Ljava/lang/Integer;.intValue:()I
move-result t13
sub-int t12, t12, t13
new-instance t11, Ljava/lang/Integer;
invoke-direct {t11, t12, Ljava/lang/Integer;.<init>:(I)V
check-cast t11, L/java/lang/Integer;
invoke-virtual {t11}, Ljava/lang/Integer;.intValue:()I
move-result t14
aput-object t1, t3, t14
const-string t15, "Hello, world!"
move-object v2, t15
invoke-static {v0, v1, v2}, print
move-result-object t16
const t18, #int 0
new-instance t17, Ljava/lang/Integer;
invoke-direct {t17, t18}, Ljava/lang/Integer;.<init>:(I)V
return-object t17
goto :L0
:L0

