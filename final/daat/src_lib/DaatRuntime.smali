.class public Luhideyuki/daat/DaatRuntime;
.super Landroid/app/Activity;
.source "DaatRuntime.java"


# static fields
.field static tv:Landroid/widget/TextView;

.field static final z:Ljava/lang/Integer;


# direct methods
.method static constructor <clinit>()V
    .registers 2

    .prologue
    .line 13
    new-instance v0, Ljava/lang/Integer;

    const/4 v1, 0x0

    invoke-direct {v0, v1}, Ljava/lang/Integer;-><init>(I)V

    sput-object v0, Luhideyuki/daat/DaatRuntime;->z:Ljava/lang/Integer;

    return-void
.end method

.method public constructor <init>()V
    .registers 1

    .prologue
    .line 10
    invoke-direct {p0}, Landroid/app/Activity;-><init>()V

    return-void
.end method

.method public static print(Ljava/lang/String;)Ljava/lang/Integer;
    .registers 2
    .param p0, "s"    # Ljava/lang/String;

    .prologue
    .line 30
    sget-object v0, Luhideyuki/daat/DaatRuntime;->tv:Landroid/widget/TextView;

    invoke-virtual {v0, p0}, Landroid/widget/TextView;->append(Ljava/lang/CharSequence;)V

    .line 31
    sget-object v0, Luhideyuki/daat/DaatRuntime;->z:Ljava/lang/Integer;

    return-object v0
.end method

.method public static tostring(Ljava/lang/Integer;)Ljava/lang/String;
    .registers 2
    .param p0, "i"    # Ljava/lang/Integer;

    .prologue
    .line 36
    invoke-virtual {p0}, Ljava/lang/Integer;->toString()Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method


# virtual methods
.method public onCreate(Landroid/os/Bundle;)V
    .registers 5
    .param p1, "savedInstanceState"    # Landroid/os/Bundle;

    .prologue
    .line 18
    invoke-super {p0, p1}, Landroid/app/Activity;->onCreate(Landroid/os/Bundle;)V

    .line 19
    const/high16 v2, 0x7f030000

    invoke-virtual {p0, v2}, Luhideyuki/daat/DaatRuntime;->setContentView(I)V

    .line 20
    const/high16 v2, 0x7f050000

    invoke-virtual {p0, v2}, Luhideyuki/daat/DaatRuntime;->findViewById(I)Landroid/view/View;

    move-result-object v2

    check-cast v2, Landroid/widget/TextView;

    sput-object v2, Luhideyuki/daat/DaatRuntime;->tv:Landroid/widget/TextView;

    .line 22
    const/16 v2, 0x400

    new-array v0, v2, [Ljava/lang/Object;

    .line 23
    .local v0, "arcd":[Ljava/lang/Object;
    new-instance v1, Ljava/lang/Integer;

    const/4 v2, -0x1

    invoke-direct {v1, v2}, Ljava/lang/Integer;-><init>(I)V

    .line 25
    .local v1, "fp":Ljava/lang/Integer;
    invoke-static {v0, v1}, Luhideyuki/daat/DaatProg;->main([Ljava/lang/Object;Ljava/lang/Integer;)Ljava/lang/Integer;

    .line 26
    return-void
.end method
