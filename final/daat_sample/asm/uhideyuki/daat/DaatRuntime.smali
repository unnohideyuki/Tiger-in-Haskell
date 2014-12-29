.class public Luhideyuki/daat/DaatRuntime;
.super Landroid/app/Activity;
.source "DaatRuntime.java"


# static fields
.field static tv:Landroid/widget/TextView;


# direct methods
.method public constructor <init>()V
    .registers 1

    .prologue
    .line 10
    invoke-direct {p0}, Landroid/app/Activity;-><init>()V

    return-void
.end method

.method public static print(Ljava/lang/String;)V
    .registers 2
    .param p0, "s"    # Ljava/lang/String;

    .prologue
    .line 29
    sget-object v0, Luhideyuki/daat/DaatRuntime;->tv:Landroid/widget/TextView;

    invoke-virtual {v0, p0}, Landroid/widget/TextView;->append(Ljava/lang/CharSequence;)V

    .line 30
    return-void
.end method


# virtual methods
.method public onCreate(Landroid/os/Bundle;)V
    .registers 5
    .param p1, "savedInstanceState"    # Landroid/os/Bundle;

    .prologue
    .line 17
    invoke-super {p0, p1}, Landroid/app/Activity;->onCreate(Landroid/os/Bundle;)V

    .line 18
    const/high16 v2, 0x7f030000

    invoke-virtual {p0, v2}, Luhideyuki/daat/DaatRuntime;->setContentView(I)V

    .line 19
    const/high16 v2, 0x7f050000

    invoke-virtual {p0, v2}, Luhideyuki/daat/DaatRuntime;->findViewById(I)Landroid/view/View;

    move-result-object v2

    check-cast v2, Landroid/widget/TextView;

    sput-object v2, Luhideyuki/daat/DaatRuntime;->tv:Landroid/widget/TextView;

    .line 21
    const/16 v2, 0x400

    new-array v0, v2, [Ljava/lang/Object;

    .line 22
    .local v0, "arcd":[Ljava/lang/Object;
    new-instance v1, Ljava/lang/Integer;

    const/4 v2, -0x1

    invoke-direct {v1, v2}, Ljava/lang/Integer;-><init>(I)V

    .line 24
    .local v1, "fp":Ljava/lang/Integer;
    invoke-static {v0, v1}, Luhideyuki/daat/DaatProg;->main([Ljava/lang/Object;Ljava/lang/Integer;)Ljava/lang/Integer;

    .line 25
    return-void
.end method
