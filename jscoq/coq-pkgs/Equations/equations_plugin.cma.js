function(OA){"use strict";var
c3=104,a4=123,dM="$l",lu="valid",bu=108,lM="Subgoal: ",lt="my_preident",c6="(",ls="Depelim",fc="src/splitting.ml",Z="Init",g7="pattern",lr="_equation_",lL="<>",mx=115,gI="|",l9="True",mw="mfix",lq="elimination",l8="This is not an equality between constructors.",fe="refine",bs="Datatypes",bN=144,l7="Program",lp="with",c5=117,dR="=>",mv="Covering",mu="Equations.Simplify",l5="binders2",l6="mkHEq",g6=128,lo="decompose_app",b4="$id",lm="uncurry_call",ln="succeeded",lK=136,e9=248,mt="pair",gH="Subterm",ll="_unfold",ms="target",l4=167,gG="P",i=107,g5="simplify",l3="?(",mr="simp",aM="Coq",fb="product",g4="by",mq="Applying ",lJ="sigmaI",aY=112,l2="simplification_rules",c4="Below",dQ="x",l1="Schemes",lk="->",mp="deppat_equations",gF="Equations_Logic",lI=105,l0="prod",mo="$i",lj=" for ",bv="src/principles_proofs.ml",e8="y",li="_where_rev",c_=152,lZ="_rec",g3="curry",cn="equations",lh="elim_patterns",lH="Equations_common",e7="dependent",mn="is_secvar",gO="Could not generate a permutation",lY=157,mm="<->",ml="False",gT="<-",lG="-",gN=139,lF="-!",gM="def",gS="Define_equations",c9="covering",gE=" := ",lE="rec",mk="funelim",gL=" and ",a3="Logic",x=109,gR="split",lX="NoConfusionPackage",g2="lident",mj="Syntax",fd=101,mi="prog",lg="struct",gQ="equation_user_option",W=125,lW="*",co="}",lf="end_of_section",v=250,bP="$c",l=246,ld=165,le="move_after_deps",e6=102,c8="Extension: cannot occur",mh=" Fail error ",lc="Extra_tactics",fa="JMeq",g1=113,b5=122,c1="EqDec",cm="{",I="",lV=149,mg="Sigma_types",lb=134,g0="FunctionalInduction",la="get_signature_pack",mf="eqns_specialize_eqs",gD="equation_options",lD="solve_equations",lC="refine_ho",gZ=103,k$="<=",lB=169,me="IDENT",gY="sigma",lA="pattern_call",e$="_ind",dP="Derive",lz="Eqdec",gK="src/sigma_types.ml",bt=" : ",md=":=!",gX="src/simplify.ml",dL=" on ",gW="$",ly="needs_generalization",gJ="Type error while building context map: ",mc=153,lx="autounfold_ref",mb=171,e5="src/equations.ml",bO=127,lU="?",c2=111,lT="deppat_elim",gP=" in context ",dN="src/covering.ml",b2=" ",k_="deppat",ma="index",b3=")",lw="interp_pats",k9="interp_pat",lR="Noconf",lS=":",lP="where ",aL="DepElim",lQ="Failed with: ",N="Equations",gC="subterm_relation",l$="_where",k8="comp",dO="_",gV="Splitting",bM="src/principles.ml",dK="Signature",c7=114,gU="wildcard",br=147,cl=":=",gB="Unnexpected goal",e_="where",lv="Relations",l_="equations_plugin",lN="g_simplification_rules",lO="Elimination",an=OA.jsoo_runtime,T=an.caml_check_bound,e4=an.caml_fresh_oo_id,bK=an.caml_make_vect,d=an.caml_new_string,u=an.caml_obj_tag,aH=an.caml_register_global,G=an.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):an.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):an.caml_call_gen(a,[b,c])}function
g(a,b,c,d){return a.length==3?a(b,c,d):an.caml_call_gen(a,[b,c,d])}function
o(a,b,c,d,e){return a.length==4?a(b,c,d,e):an.caml_call_gen(a,[b,c,d,e])}function
L(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):an.caml_call_gen(a,[b,c,d,e,f])}function
c0(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):an.caml_call_gen(a,[b,c,d,e,f,g])}function
aF(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):an.caml_call_gen(a,[b,c,d,e,f,g,h])}function
k7(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):an.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
ck(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):an.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
Oz(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):an.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}function
bL(a,b,c,d,e,f,g,h,i,j,k,l,m){return a.length==12?a(b,c,d,e,f,g,h,i,j,k,l,m):an.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l,m])}var
q=an.caml_get_global_data(),fi=d(N),d1=[0,d("Lists"),[0,d("List"),0]],fr=d(gC),cw=[0,[0,0],1],am=d(l_),t=q.CamlinternalLazy,aO=q.Universes,R=q.Constr,r=q.Termops,c=q.EConstr,P=q.Assert_failure,j=q.Names,aP=q.Univ,_=q.Inductiveops,e=q.Util,hY=q.Reduction,b$=q.Term,ad=q.Typeclasses,au=q.Globnames,aI=q.Hints,B=q.Evarutil,w=q.Evd,af=q.CErrors,f=q.Pp,at=q.Feedback,C=q.Context,H=q.Option,bj=q.Printer,k=q.Proofview,cz=q.Refiner,h2=q.Pretype_errors,h3=q.Himsg,y=q.Tacmach,s=q.Tactics,aN=q.Libnames,M=q.Ltac_plugin,aA=q.Environ,U=q.Reductionops,A=q.Global,aC=q.Coqlib,S=q.CAst,n=q.Genarg,D=q.Stdarg,Y=q.Geninterp,ao=q.Nametab,X=q.Not_found,m=q.Tacticals,dZ=q.Evarsolve,aV=q.Namegen,bg=q.Declare,E=q.Stdlib,he=q.Flags,aq=q.Typing,hd=q.Univops,cr=q.Smartlocate,g9=q.Stdlib__printexc,g_=q.Stdlib__printf,cq=q.Goptions,hg=q.Stdlib__lazy,ax=q.Retyping,dn=q.Dumpglob,p=q.Stdlib__list,fE=q.CString,bS=q.Ppconstr,il=q.DAst,a0=q.Constrintern,J=q.Loc,cF=q.Constrexpr_ops,cd=q.Impargs,O=q.Int,fJ=q.Failure,i8=q.Goal,bd=q.Invalid_argument,K=q.Nameops,a_=q.Tacred,F=q.CList,bm=q.CArray,aJ=q.Evar,be=q.CClosure,cf=q.Refine,cO=q.Inductive,dv=q.Locusops,cN=q.Extraction_plugin,ji=q.ComInductive,aT=q.Obligations,bV=q.Lemmas,a1=q.Vars,aK=q.Stdlib__array,eI=q.Evarconv,cV=q.Equality,ge=q.Class_tactics,eN=q.Conv_oracle,ci=q.Autorewrite,kc=q.Indschemes,kb=q.Sorts,ki=q.Find_subterm,ku=q.Metasyntax,kt=q.Pretyping,b1=q.Ftactic,h=q.Pcoq,gv=q.Egramml,cX=q.Vernac_classifier,gu=q.Vernacinterp,a$=q.Genintern,aX=q.Mltop,bo=q.CLexer,o4=[0,d("src/equations_common.ml"),527,9],p3=[0,0,0],p2=[0,0,0],p1=[0,0,0,0,0],pV=d(lM),pU=d(ln),pP=d(dL),pQ=d(lj),pR=d(mh),pT=d(" Pretype error: "),pS=d(lQ),pN=d(dL),pO=d(mq),pK=d(" depends"),pL=d("Found no hypothesis on which "),pM=[0,d("move_before_deps")],pD=[0,0],px=d("Equations.DepElim.depind"),pv=d("Equations.DepElim.simpl_dep_elim"),pu=d("Equations.DepElim.depelim_nosimpl"),pt=d("Equations.DepElim.do_empty"),pr=d("Equations.DepElim.depelim"),pq=d("Equations.DepElim.impossible_call"),pi=d(dQ),ph=d("Equations.FunctionalInduction.specialize_mutfix"),pg=d("Equations.DepElim.simpl_equations"),pf=d("Equations.EqDecInstances.eqdec_proof"),pe=d("Equations.NoConfusion.solve_noconf"),pd=d("Equations.Subterm.pi"),pc=d("Equations.DepElim.find_empty"),pb=d("Equations.Equations.solve_rec"),pa=d("Equations.DepElim.set_eos"),o$=d("Equations.DepElim.equations"),o_=d("Equations.Subterm.unfold_recursor"),o9=d("Equations.Subterm.rec_wf_eqns_rel"),o8=d("Equations.Below.rec"),o1=d("Unknown database "),o2=[0,d("autounfold")],o3=d("Nothing to unfold"),o0=[0,0,0],oX=d("pr2"),oY=[0,d(N),[0,d(Z),0]],oV=d("pr1"),oW=[0,d(N),[0,d(Z),0]],oT=d(lJ),oU=[0,d(N),[0,d(Z),0]],oR=d(gY),oS=[0,d(N),[0,d(Z),0]],oK=d(lf),oL=[0,d(N),[0,d(aL),0]],oD=d("add_pattern"),oE=[0,d(N),[0,d(aL),0]],oB=d("hidebody"),oC=[0,d(N),[0,d(Z),0]],oz=d("hide_pattern"),oA=[0,d(N),[0,d(aL),0]],ow=d("block"),ox=[0,d(N),[0,d(aL),0]],ou=d("inaccessible_pattern"),ov=[0,d(N),[0,d(aL),0]],os=d(lX),ot=[0,d(N),[0,d(aL),0]],n_=d("DependentEliminationPackage"),n$=[0,d(N),[0,d(aL),0]],n8=d("FunctionalElimination"),n9=[0,d(N),[0,d(g0),0]],n6=d(g0),n7=[0,d(N),[0,d(g0),0]],n4=[0,1],n0=d(l0),n1=[0,d(Z),[0,d(bs),0]],n2=d(fb),nW=d(mt),nX=[0,d(Z),[0,d(bs),0]],nY=d(fb),nT=d("and"),nU=[0,d(Z),[0,d(a3),0]],nV=d(fb),nP=d("conj"),nQ=[0,d(Z),[0,d(a3),0]],nR=d(fb),nM=d("Id_rect_dep_r"),nN=[0,d(N),[0,d(aL),0]],nJ=d("Id_rect_r"),nK=[0,d(N),[0,d(aL),0]],nG=d("id_refl"),nH=[0,d(N),[0,d(Z),0]],nD=d("Id"),nE=[0,d(N),[0,d(Z),0]],nA=d("Empty"),nB=[0,d(N),[0,d(Z),0]],ny=d("fixproto"),nz=[0,d(N),[0,d(Z),0]],nv=d("JMeq_refl"),nw=[0,d(a3),[0,d(fa),0]],nx=d(l6),ns=d(fa),nt=[0,d(a3),[0,d(fa),0]],nu=d(l6),np=d("eq_rect_dep_r"),nq=[0,d(N),[0,d(aL),0]],nl=d("eq_rect_r"),nm=[0,d(Z),[0,d(a3),0]],nn=d("coq_eq_case"),nj=d("nat"),nk=[0,d(Z),[0,d(bs),0]],nh=d("S"),ni=[0,d(Z),[0,d(bs),0]],nf=d("O"),ng=[0,d(Z),[0,d(bs),0]],m_=d(ml),m$=[0,d(aM),[0,d(Z),[0,d(a3),0]]],m7=d("I"),m8=[0,d(aM),[0,d(Z),[0,d(a3),0]]],m4=d(l9),m5=[0,d(aM),[0,d(Z),[0,d(a3),0]]],m2=d("tt"),m3=[0,d(aM),[0,d(Z),[0,d(bs),0]]],m0=d("unit"),m1=[0,d(aM),[0,d(Z),[0,d(bs),0]]],mZ=[1,10],mX=[0,0],mY=d(" is defined"),mV=d(cn),mR=[0,[11,d("Exception while typechecking context "),[2,0,[11,d(bt),[2,0,[12,10,0]]]]],d("Exception while typechecking context %s : %s\n")],mC=[0,d(N),[0,d("OCaml"),[0,d(gV),0]]],mD=d("splitting variables in OCaml"),mG=[0,d(N),[0,d("WithK"),0]],mH=d("using K during simplification"),mK=[0,d(N),[0,d("Transparent"),0]],mL=d("leave definitions transparent"),mO=[0,d(N),[0,d("Debug"),0]],mP=d("Equations debug output"),mW=[0,0],nb=d(l0),nc=[0,d(aM),[0,d(Z),[0,d(bs),0]]],nd=d(mt),ne=[0,d(aM),[0,d(Z),[0,d(bs),0]]],n5=[0,d(N),[0,d(N),0]],oa=[0,d(N),[0,d(c4),0]],ob=d("WellFounded"),oc=[0,d(N),[0,d("Classes"),0]],od=d("well_founded"),oe=[0,d(aM),[0,d(Z),[0,d("Wf"),0]]],of=d("relation"),og=[0,d(aM),[0,d(lv),[0,d("Relation_Definitions"),0]]],oh=d("clos_trans"),oi=[0,d(aM),[0,d(lv),[0,d("Relation_Operators"),0]]],oj=d("id"),ok=[0,d(aM),[0,d(Z),[0,d(bs),0]]],om=d("list"),oo=d("nil"),oq=d("cons"),oF=d("eos"),oG=d("the_end_of_the_section"),oH=[0,d(N),[0,d(aL),0]],oI=d(lf),oJ=[0,d(N),[0,d(aL),0]],oM=d("notT"),oN=[0,d(aM),[0,d(Z),[0,d("Logic_Type"),0]]],oP=d("ImpossibleCall"),oQ=[0,d(N),[0,d(aL),0]],o5=[0,d(c4),[0,d(N),0]],pj=[0,d(aL),[0,d(N),0]],pm=d("solve_equation"),p_=d("No derive declared for "),p8=d("Expected an inductive type"),p$=d("#"),qa=d(b3),qb=d(c6),qc=d(b3),qd=d(l3),qf=d(cl),qg=d(md),qh=d(co),qi=d(cm),qj=d("rec "),qk=d(dR),ql=d(co),qm=d(cm),qn=d(dR),qo=d(k$),qp=d(co),qq=d(cm),qr=d(g4),qs=d(co),qt=d(cm),qu=d(g4),qv=d(bt),qy=d(gU),qz=d("Aliases not supported by Equations"),qA=[0,d(lw)],qD=d(gU),qE=d(" cannot be applied"),qF=d("Pattern variable "),qG=[0,d(k9)],qH=d("Or patterns not supported by Equations"),qI=d(k9),qL=d("Expecting a pattern for "),qM=d(lw),qN=d(gM),qO=d(lL),qP=d(gM),qQ=d(lL),qJ=d("Non-linear occurrence of variable in patterns"),qK=[0,d("ids_of_pats")],qC=[0,0,0],qx=[0,0,0],qT=d(" appears twice"),qU=d("Non-linear pattern: variable "),qV=d(gU),ri=[0,d(dN),315,13],rG=[0,0,0,0],rH=[0,0,0],rI=[0,0,0],sp=d("Non-exhaustive pattern-matching, no clause found for:"),sq=d(k_),sK=d("covering_aux: unexpected output of tactic call"),sL=d(" refining "),sM=d("Unable to find a covering for the result of a by clause:"),sN=[0,d(c9)],sF=d("And the user patterns are: "),sG=d("Problem is "),sH=d("Non-matching clause in with subprogram:"),sI=[0,d(c9)],sJ=[0,d(dN),1484,52],sE=d("unknown"),sz=d("This pattern must be innaccessible and equal to "),sA=[0,d(c9)],st=d("is not inaccessible, but should refine pattern "),su=d("Pattern "),sv=[0,d(c9)],sw=d("should be convertible to "),sx=d("Incompatible innaccessible pattern "),sy=[0,d(c9)],sr=d("Unbound variable "),ss=d(cn),sB=d(fe),sC=[0,1],sD=[0,0],sP=[0,0],sO=d("interp_wheres"),sQ=d("Unable to build a covering for:"),sR=d(k_),sn=d("Unused clause "),so=d(c9),sl=d("In context: "),sk=d(b2),sj=d("named_of_rel_context"),sf=[0,d(dN),1094,19],se=[0,d(dN),1083,21],rT=d("*impossible case*"),rM=d(gE),rN=d(bt),rO=d(lP),rP=d(gP),rQ=d(bt),rR=d(gE),rS=d(" :=! "),rU=d(gP),rV=d(bt),rW=d(" split: "),rX=d(gP),rY=d("Valid "),rZ=d("Mapping "),r0=d("RecValid "),r1=d("New problem to problem substitution is: "),r2=d("Revctx is: "),r3=d(" eliminating "),r4=d(" for type "),r5=d("New problem: "),r6=d(" in "),r7=d(b2),r8=d(b2),r9=d(bt),r_=d(b2),r$=d(" refine "),rL=d(lS),rJ=[0,0,0],rD=d("Occurs check singleton subst"),rC=[0,d(dN),643,11],rB=[0,0],rA=[0,1],rx=d(gL),ry=d("Contexts do not agree for composition: "),rz=d("check_eq_context_nolet"),rw=[0,[11,d("Exception while comparing contexts "),[2,0,[11,d(gL),[2,0,[11,d(bt),[2,0,[12,10,0]]]]]]],d("Exception while comparing contexts %s and %s : %s\n")],rv=[0,0],rt=d("split_tele"),rs=d("split_context"),ro=d(gO),rm=[0,0],rn=[0,0],rl=d(gO),rk=d(gO),rj=[0,0,0],rh=[0,0,0],rb=d(gJ),rc=d(cn),rd=d("Invalid_argument: "),re=d(gJ),rf=d(cn),q_=d("Anomaly: "),q$=d(gJ),ra=d(cn),q9=[0,0],q4=d(bt),q5=d(b2),q6=d("|-"),q7=d(b2),q3=d(b2),q1=d("; "),qY=d(cl),q0=d(dO),qZ=d(bt),qW=d(b3),qX=d(c6),qR=[0,0],qS=[0,1],rE=d("Equations.Covering.Conflict"),rF=d("Equations.Covering.Stuck"),tv=d("Goal cannot be curried"),tt=d("No currying to do in "),tk=[0,d(gK),474,14],tl=[0,d(gK),512,17],tr=[0,1],tp=d("Could not revert a cut, please report."),tm=[0,1],tn=[0,1],to=[0,1],tq=[0,1],tf=d(".] to avoid this."),tg=d(". Use [Derive Signature for "),th=d("Automatically inlined signature for type "),s9=d("_sig"),s_=[1,0],s$=d("_sig_pack"),ta=d("_var"),tb=[1,0],tc=d("_Signature"),s7=d("No signature to derive for non-dependent inductive types"),s8=d("Derive Signature"),s4=d("signature_pack"),s5=[0,d(N),[0,d(dK),0]],s1=d("signature"),s2=[0,d(N),[0,d(dK),0]],sZ=d(dK),s0=[0,d(N),[0,d(dK),0]],sX=d(ma),sY=d(ma),sW=[0,d(gK),85,10],sU=d("constrs_of_coq_sigma"),te=d(dK),tS=[0,0,1],tN=d("c"),tO=d(gG),tP=d("step"),tQ=d(lE),tR=d("below"),tT=d("Below_"),tU=[1,0],tV=d("below_"),tW=[1,0],tJ=[0,d("src/subterm.ml"),229,55],tG=d("_subterm"),tH=d("well_founded_"),tK=d(e8),tL=d(dQ),tI=[1,0],tE=d(dO),tF=d(dO),tD=d("_direct_subterm"),tA=d(dQ),tB=d(e8),tC=d("z"),tz=d("Equations.Subterm.solve_subterm"),tM=d(gH),tX=d(c4),t9=d("_eqdec"),t7=[1,10],t8=d("_EqDec"),t3=d(dQ),t4=d(e8),t1=d("dec_eq"),t2=[0,d(N),[0,d(c1),0]],tZ=d(c1),t0=[0,d(N),[0,d(c1),0]],tY=d("param"),t_=d(c1),t$=d(dQ),ua=d(e8),ub=d("NoConfusion_"),uc=d("noConfusion_"),ud=d("NoConfusionPackage_"),ue=[1,0],uf=[0,d("src/noconf.ml"),gN,11],ug=d("NoConfusion"),vO=d(mu),v1=d(lU),v2=d(mm),v3=d(lW),vR=d("NoConfusionOut"),vS=d("NoCycle"),vT=d("ElimTrue"),vU=d("ElimFalse"),vW=d(lF),vV=d(lG),vY=d(gT),vX=d(lk),vZ=d(gW),vP=[0,d(gX),903,19],vQ=[0,1],vF=[1,0],vG=[1,1],vH=d("Neither side of the equality is a variable."),vI=[1,0],vJ=[1,1],vK=[0,0],vL=d("Could not infer a simplification step."),vE=d("[elim_true] The first hypothesis is not the empty type."),vC=d("[elim_true] The first hypothesis is not the unit type."),vD=[0,0,0],vB=d("[noCycle] is not implemented!"),vy=d("Expected a full application of [opaque_ind_pack_eq_inv]. Maybeyou did not solve completely a NoConfusion step?"),vz=d("[opaque_ind_pack_eq_inv] should be applied to [eq_refl]."),vA=[0,0,0],vw=d("[noConfusion] Cannot find an instance of NoConfusion for type "),vx=[0,0,0],vr=d(l8),vs=d(l8),vt=d("[noConfusion] Cannot simplify without K on type "),vu=[0,0,0],vp=d("[solution] The variable appears on both sides of the equality."),vl=[0,1],vo=[0,0,0],vm=[0,0,0],vn=d("[deletion] Cannot simplify without K on type "),vh=[0,0,0],vg=[0,0,0],vi=[0,0,0],vj=d("If you see this, please report."),va=d("The first hypothesis in the goal is not an equality."),vb=d("The first hypothesis in the goal is not an equality between identical terms."),vc=d("The left-hand side of the first hypothesis in the goal is not a variable."),vd=d("The right-hand side of the first hypothesis in the goal is not a variable."),u$=d("The goal is not a product."),u8=[0,d(gX),277,12],u6=d("Unexpected mismatch."),u4=[0,d(gX),218,30],ui=[0,d(N),[0,d(aL),0]],uh=d(mu),uk=d(c1),ul=[0,d(N),[0,d(c1),0]],un=d(ml),uo=[0,d(aM),[0,d(Z),[0,d(a3),0]]],up=d(l9),uq=[0,d(aM),[0,d(Z),[0,d(a3),0]]],ur=d("True_rect_dep"),us=d("False_rect"),ut=[0,d(aM),[0,d(Z),[0,d(a3),0]]],uv=d("False_rect_dep"),ux=d(lX),uy=[0,d(N),[0,d(aL),0]],uA=d("apply_noConfusion"),uB=d("simplify_ind_pack"),uC=d("simplify_ind_pack_inv"),uD=d("opaque_ind_pack_eq_inv"),uE=d("eq_simplification_sigma1"),uF=d("eq_simplification_sigma1_dep"),uG=d("eq_simplification_sigma1_dep_dep"),uH=d("simplification_K"),uI=d("simplification_K_dec"),uJ=d("solution_left"),uL=d("solution_left_dep"),uN=d("solution_right"),uP=d("solution_right_dep"),uR=d(gY),uS=[0,d(N),[0,d(Z),0]],uT=d(lJ),uU=[0,d(N),[0,d(Z),0]],u3=d("Equations.Simplify.CannotSimplify"),wr=[0,d(fc),469,13],wq=[0,d(fc),470,24],wo=[0,[0,1,0]],wp=d("_functional"),we=d("m_"),v$=d("Simplifying term:"),wa=d("... in context:"),wb=[0,1],wc=[0,1],wd=d("Should not fail here, please report."),v4=[0,d(fc),110,22],v5=d(mi),v6=[0,0],v9=[0,d(fc),119,7],v7=d(gR),v8=[3,[0,0],0],v_=[0,[0,0,2],0],wf=[0,0,0],wg=d("branches"),wh=d(ms),wi=[0,0],wj=d(mi),wk=[0,0],w3=d("Not enough products in "),w4=d("Cannot do a fixpoint on a non inductive type."),xE=d("Unexpected refinement goal in functional induction proof"),xF=d("clear body"),xG=d("convert concl"),xH=d("letin"),xB=d("Unexpected goal in functional induction proof"),xy=[0,d(bv),380,34],xp=[0,d(bv),381,35],xx=d(l$),xq=d("one where"),xr=d("Couldn't find associated args of where"),xs=d(" context map "),xt=d(" instance: "),xu=d("term: "),xv=d(" where: "),xw=d("Found path "),xz=d(li),xA=d("whererev"),xk=d("solving premises of compute rule"),xl=d("applying compute rule"),xm=d("wheretac"),xn=d("compute "),xo=d("compute empty"),xC=d(gR),xD=d(lu),xI=d(fe),ym=[0,d(bv),821,14],yl=d(mk),yn=d("exception"),yh=d("Unexpected unfolding lemma goal"),yd=d("Unexpected unfolding goal"),x7=[1,[0,1,0]],x8=[1,[0,1,0]],x6=[0,d(bv),718,16],x9=d(e_),x_=[0,d(bv),702,28],x5=[0,d(bv),703,29],yb=[0,d(bv),740,9],x$=d("compute rhs"),ya=d("compute"),ye=d(gR),yf=d(lu),yg=d("recvalid"),yi=d("refined"),yc=[0,d(bv),753,14],x2=[1,[0,1,0]],x3=[1,[0,1,0]],x1=d(ll),x4=d("solve_eq"),yj=[1,[0,1,0]],yk=[1,[0,1,0]],xS=[0,d(bv),567,25],xT=d("after mut -> nested and mut provable"),xU=d("mutfix"),xV=d("spliting nested"),xW=d("assert mut -> nest first subgoal "),xX=d(lZ),xY=[0,0,1],xR=d(mk),xZ=[0,0,0],xQ=d(lM),xP=d(ln),xL=d(dL),xM=d(lj),xN=d(mh),xO=d(lQ),xJ=d(dL),xK=d(mq),xj=[0,1],xh=[0,[0,0],1],xi=[0,10],xa=d("Fixpoints should be on the same mutual inductive declaration."),xb=d(" already used in the environment"),xc=d("Name "),xd=[0,d("Logic.prim_refiner")],xe=[0,d(bv),205,29],xg=d("fix_"),w5=d(" subgoal"),w6=d(gL),w7=d(" index"),xf=d(" indices"),w8=d(b2),w9=d(" name"),w_=d("Cannot apply mutual fixpoint, invalid arguments: "),w$=[0,d(mw)],w0=d(dL),w1=d("Trying "),wZ=d("failed"),w2=d("Couldn't rewrite"),wW=d("_wf_obligations"),wV=[0,d(gC),[0,d(c4),[0,d("rec_decision"),0]]],wQ=d(c4),wP=d("Helper not found while proving induction lemma."),y6=[0,0,0],zj=d(lr),zk=d("_helper_"),ze=d("_mut"),y$=[0,d(lO),[0,d(l1),0]],za=[0,1],zb=[0,d(lO),[0,d(l1),0]],zc=[0,1],zd=d(e$),zh=d(lZ),zi=d("_rect"),zg=d("_ind_comb"),zf=d(e$),y9=d(lr),y8=d("_refinement_"),y7=d(e$),y_=d("_ind_"),y3=d("FunctionalInduction_"),y0=d(I),y2=d(dO),y1=d(e$),yZ=d("_ind_fun"),y5=[0,d(bM),946,12],y4=d("Induction principle could not be proved automatically: "),yV=d("FunctionalElimination_"),yX=d("_ind_ind"),yW=d("_elim"),yS=[0,d(bM),762,22],yR=[0,0,0],yT=[0,0,1],yU=[0,1,1],yP=d("_unfold_eq"),yQ=[0,-1],yO=[1,[0,1,0]],yL=d(gE),yM=d(bt),yN=d(lP),yJ=[0,1,0],yI=[0,d(bM),496,16],yF=[0,0],yE=d("Heq"),yC=d("refine_eq"),yD=d(fe),yz=d("abs"),yA=[0,d(bM),413,8],yB=[0,0,0],yG=[0,d(bM),452,30],yH=[0,d(bM),446,9],yy=[0,d(bM),309,10],yx=[0,d(bM),158,15],yw=[0,0,0],yv=[0,1],yu=d("Hind"),yt=[0,0,0],yr=[0,d(bM),68,12],yo=[0,1],yp=[0,0],zL=d("Generated clauses: "),zM=d("dummy"),zI=[0,d("src/depelim.ml"),523,19],zJ=[0,1],zK=[12,0,0,0],zN=d("Could not eliminate variable "),zO=d("No such hypothesis: "),zH=d("Specialization not allowed on dependent hypotheses"),zG=d("Nothing to do in hypothesis "),zE=d("destPolyRef"),zD=[0,0],zA=d("DependentElimination_"),zw=d(gG),zx=d(gG),zy=d("_dep_elim"),zz=[1,7],zv=[0,0],zu=[0,1],zs=[0,d(aM),[0,d(a3),[0,d(fa),0]]],zt=[0,1],zp=[0,1],zo=d("gen_x"),zq=[0,1],zn=[0,1],zm=[0,1],zl=d("Equations.Depelim.Seen"),zC=d("DependentElimination"),Az=d(gB),Ax=d(ms),AA=d(gB),Ay=d(gB),Aw=d(gM),Au=[0,d(e5),584,63],As=[1,0],Ao=[0,0],Ab=d(" found"),Ac=d("No argument named "),Ad=d("struct_index"),Ap=[0,0],Aq=[0,[1,0]],Ar=[0,0],Ae=d("_comp"),Af=[1,0],Ag=d(c4),Ah=d("program"),Ai=d(gC),Aj=d("_comp_proj"),Ak=d(k8),Al=[1,0],Am=[0,1,0,1],An=[0,[0,0]],z_=[0,0],z$=[3,0],Aa=[2,1],Av=[0,d(e5),391,11],At=[0,0],z9=[1,5],z8=[1,5],z5=d(l$),z6=d(li),z3=[0,d(e5),156,67],z4=d("_eq"),z2=d(ll),z7=[0,0],zV=[0,0],zW=[0,1],zX=[0,0],zY=[0,1],zZ=[0,0],z0=[0,1],z1=[0,0],zR=d("fix_proto"),zS=[0,d(l7),[0,d("Tactics"),0]],zT=[0,d(e5),48,9],zP=d(l7),AH=d("Products do not match"),AG=d("Second-order matching failed"),AF=d("Couldn't find a second-order pattern to match"),AC=[0,d("src/extra_tactics.ml"),19,11],AD=d("core"),AB=d("f"),Nn=[2,0],Nh=[0,1],Nb=[0,0],Mt=d(dP),Mh=d(dP),Me=d(c8),Mc=d(dP),L$=d(c8),LB=d("Not a section variable or hypothesis"),Lr=[0,0,0],Le=[0,0,0],K6=d(gS),KX=d(gS),KU=d(c8),KS=d(gS),KP=d(c8),HD=[0,d("src/g_equations.ml4"),368,20],Ef=[3,0],Eb=[3,1],D9=[2,0],D5=[2,1],D1=[1,0],DT=[0,1],DP=[0,0],C3=d("No generalization needed"),C0=d(gF),Cs=d(gF),Cp=d(c8),Cn=d(gF),Ck=d(c8),B4=[0,0],AK=d(bP),AO=d("$h'"),AS=d("$h"),AV=d(lo),AX=d(lo),A0=d("$myref"),A3=d(lx),A5=d(lx),A8=d("$id'"),Ba=d(b4),Bd=d(la),Bf=d(la),Bi=d(b4),Bl=d(gY),Bm=d(g7),Bo=d("pattern_sigma"),Bq=[0,d(g3),0],Bt=d(b4),Bw=d(g3),By=d(g3),BB=d(b4),BE=d("uncurry_hyps"),BG=d("curry_hyps"),BJ=d(b4),BN=d(bP),BQ=d(lm),BS=d(lm),BV=d(bP),BY=d(g7),BZ=d(e7),B1=d("dependent_pattern"),B5=d(bP),B8=d("from"),B9=d(g7),B_=d(e7),Ca=d("dependent_pattern_from"),Cd=d(bP),Cg=d(lA),Ci=d(lA),CW=[0,d(a3)],CX=[0,d(N)],C4=d(b4),C7=d(ly),C9=d(ly),Da=d("$tac"),De=d("$destruct"),Dh=d(lD),Dj=d(lD),Dm=d(bP),Dq=d(dM),Dt=d("simpc"),Dw=d(bP),DA=d(dM),DD=d(mr),DF=d(mr),DG=d(gQ),DL=d(gQ),DQ=d("noind"),DU=d("ind"),DY=d(lg),D2=d("nostruct"),D6=d(k8),D_=d("nocomp"),Ec=d("eqns"),Eg=d("noeqns"),Ek=d(gQ),El=d(gD),Eq=d(gD),Eu=d(b3),Ew=d(c6),EB=d(gD),EC=d(g2),EH=d(g2),EN=d(g2),EQ=d(l5),ES=d(l5),EU=d(mp),EX=d(mp),EY=d(lT),E1=d(lT),E2=d(cn),E5=d(cn),E7=d(lt),E_=d(lt),E$=d("identloc"),Fa=d("equation"),Fb=d("ipatt"),Fc=d("patt"),Fd=d("pat_head"),Fe=d("lpatt"),Ff=d(fe),Fg=d("struct_annot"),Fh=d("rec_annot"),Fi=d("where_clause"),Fj=d(e_),Fk=d("rhs"),Fl=d("sub_equations"),Fp=[0,d(me),d(I)],FC=[0,d(I),d(";")],FL=[0,d(I),d("]")],FN=[0,d(I),d(gI)],FR=[0,d(I),d("[")],Ge=[0,d(I),d(gI)],Gi=[0,d(I),d(gI)],Gr=[0,d(I),d(co)],Gu=[0,d(I),d(cl)],Gx=[0,d(I),d(cm)],GQ=[0,d(I),d(dO)],GV=[0,d(I),d(b3)],GY=[0,d(I),d(c6)],G5=[0,d(I),d(b3)],G8=[0,d(I),d(l3)],Hd=d("0"),HE=[0,d(I),d(",")],HN=[0,d(I),d(b3)],HQ=[0,d(I),d(lg)],HS=[0,d(I),d(c6)],H5=[0,d(I),d(e_)],Ia=[0,d(I),d(lp)],Il=[0,d(I),d(cl)],Io=[0,d(I),d(lS)],IG=[0,d(I),d(e_)],IR=[0,d(I),d(md)],I1=[0,d(I),d(cl)],I6=[0,d(I),d(dR)],Jh=[0,d(I),d(cl)],Jm=[0,d(I),d(dR)],Ju=[0,d(I),d(lp)],Jz=[0,d(I),d(k$)],JL=[0,d(I),d(b3)],JO=[0,d(I),d(c6)],JQ=[0,d(I),d(gT)],J2=[0,d(I),d(cl)],J7=[0,d(I),d(dR)],Kf=[0,d(me),d(lE)],Kh=[0,d(I),d(g4)],Ku=[0,d(I),d(co)],Kx=[0,d(I),d(cm)],K3=[0,d(N)],K7=d(lh),Lb=d(lh),Lf=d(dM),Li=d("as"),Lk=d(b4),Ln=d(lq),Lo=d(e7),Ls=d(b4),Lv=d(lq),Lw=d(e7),Ly=d("dependent_elimination"),LC=d("$x"),LF=d(mn),LH=d(mn),LK=d(bP),LN=d(lC),LP=d(lC),LS=d(mo),LV=d(mf),LX=d(mf),L0=d(bP),L4=d(mo),L7=d(le),L9=d(le),Ml=[0,d("for")],Mq=[0,d(dP)],Mu=d(lN),Mx=d(lN),My=d("simplification_rule_located"),Mz=d("simplification_rule"),MA=d("simplification_step"),MB=d("direction"),MW=[0,d(I),d(lU)],M1=[0,d(I),d(mm)],M6=[0,d(I),d(lW)],Nc=[0,d(I),d(lG)],Ni=[0,d(I),d(lF)],No=[0,d(I),d(gW)],Nt=[0,d(I),d(co)],Nw=[0,d(I),d(cm)],Ny=[0,d(I),d(gW)],NM=[0,d(I),d(lk)],NR=[0,d(I),d(gT)],NV=d(l2),N1=d(l2),N3=[0,d(g5),0],N6=d(dM),N9=d(g5),N$=d(g5),Oc=d(dM),Og=d("$li"),Oj=d(mw),Ol=d("mutual_fix"),Om=d(lH),On=d(mg),Oo=d(lz),Op=d(gH),Oq=d(ls),Or=d(mj),Os=d(mv),Ot=d(gV),Ou=d(lR),Ov=d(lc),Ow=d(N),Ox=d("equations_plugin_mod"),Oy=d(l_),p7=q.Evardefine,qB=q.Lib,sa=q.Topfmt,ti=q.Patternops,t5=q.Safe_typing,t6=q.Future,u5=q.Typeops,ys=q.Stdlib__map,AE=q.Eauto;function
my(d,c,b){return a(d,a(c,b))}function
mz(d,c,b){var
e=b[1],f=a(c,b[2]);return[0,a(d,e),f]}function
mA(a){return a}function
c$(b){var
d=b[1];return[0,d,a(c[2][1],b[2])]}function
bQ(d,a){var
e=a[1];return[0,e,b(c[2][2],d,a[2])]}var
cp=[0,1],dS=[0,1],dT=[0,0];function
mB(a){cp[1]=a;return 0}var
mE=[0,0,mD,mC,function(a){return cp[1]},mB];b(cq[4],0,mE);function
mF(a){dS[1]=a;return 0}var
mI=[0,0,mH,mG,function(a){return dS[1]},mF];b(cq[4],0,mI);function
mJ(a){dT[1]=a;return 0}var
mM=[0,0,mL,mK,function(a){return dT[1]},mJ];b(cq[4],0,mM);var
bf=[0,0];function
mN(a){bf[1]=a;return 0}var
mQ=[0,0,mP,mO,function(a){return bf[1]},mN];b(cq[4],0,mQ);function
ff(d,c,b,a){return o(aq[5],d,[0,c],b,a)}function
g8(c,b,a){g(aq[4],c,[0,b],a);return 0}function
fg(j,i,h){try{var
d=function(d,e){g8(e,i,a(C[1][1][3],d));var
f=a(C[1][1][2],d);function
g(b){return ff(e,i,b,a(C[1][1][3],d))}b(H[13],g,f);return b(c[bu],d,e)};g(e[17][19],d,h,j);var
o=0;return o}catch(d){d=G(d);var
k=a(g9[1],d),l=b(c[x],h,j),m=a(r[g6],l),n=a(f[49],m);g(g_[3],mR,n,k);throw d}}function
fh(d){var
b=a(c[8],R[6]);return ck(B[5],aA[28],w[16],0,0,0,0,0,0,b)[2]}function
z(b){return a(k[70][8],b)}function
aB(a){return b(k[70][1],0,a)}function
g$(c){var
d=[0,c,0];function
f(g,c){var
d=c[1],f=b(e[18],c[2],[0,d,0]);return[0,a(e[17][6],d),f]}return g(e[17][19],f,c,d)[2]}function
ha(e){return function(g,f){var
c=g,a=f;for(;;){if(a){var
h=a[2],d=b(e,c,a[1]);if(d)return d;var
c=c+1|0,a=h;continue}return 0}}}function
mS(a){return g(e[19][7],a,0,a.length-1-1|0)}function
mT(a){return b(e[19][54],a.length-1-1|0,a)}function
hb(e,d){return function(f){var
a=f;for(;;){if(a){var
c=a[1],g=a[2],h=c[1];if(b(e,d,c[2]))return h;var
a=g;continue}throw X}}}function
mU(c,b){var
d=0;function
f(d,b){var
e=a(c,d);function
f(a){return[0,a,b]}return g(H[24],f,b,e)}var
h=g(e[19][18],f,b,d);return a(e[19][12],h)}function
a5(c,a){return b(B[13],c,a)}function
ar(a,c){var
b=a5(a[1],c),d=b[2];a[1]=b[1];return d}function
as(c,b,a){return ar(a,g(aC[1],fi,c,b))}function
ac(b,a){return g(aC[1],fi,b,a)}function
dU(b,a){return g(aC[2],mV,b,a)}function
fj(b){var
c=a(aN[34],b),d=a(ao[10],c);return a(cr[2],d)}function
hc(b){var
d=a(aN[34],b),e=a(ao[9],d),f=a(aO[50],e);return a(c[8],f)}var
fk=a(aq[3],mW);function
dV(y,j,d,f,i){var
n=j?j[1]:0,e=a(A[2],0);o(aq[3],0,e,d,i);var
p=f?(o(aq[3],0,e,d,f[1]),d[1]):d[1],h=a(B[47],p)[1],k=b(c[5],h,i),q=a(c[5],h),l=b(H[16],q,f),r=b(hd[1],e,k),s=aP[2][1],t=a(hd[1],e),u=g(H[24],t,s,l),v=b(aP[2][7],r,u),m=b(w[124],h,v);d[1]=m;var
x=[0,b(w[c_],n,m)];return aF(bg[2],0,0,0,l,x,0,k)}function
bw(c,i,h,g,e,d){var
k=[0,[0,dV(mX,[0,g],[0,e],h,i)],d],l=L(bg[3],0,0,c,0,k),m=a(j[1][8],c),n=b(E[17],m,mY),o=a(f[3],n),p=at[6];function
q(a){return b(p,0,a)}b(he[25],q,o);return l}function
da(k,j,i,e,d,h){var
f=b(ad[16],d,h),l=f[2],m=a(H[7],f[1]),n=b(c[38],m,e),g=bw(k,n,[0,b(c[37],l,e)],j,i,mZ),p=o(ad[5],d[1],aI[4],1,[1,g]);a(ad[6],p);return a(c[22],g)}var
ah=[l,function(a){return ac(m1,m0)}],bh=[l,function(a){return ac(m3,m2)}],m6=[l,function(a){return ac(m5,m4)}],m9=[l,function(a){return ac(m8,m7)}],na=[l,function(a){return ac(m$,m_)}];function
dW(a){return as(nc,nb,a)}function
fl(a){return as(ne,nd,a)}var
db=[l,function(a){return dU(ng,nf)}],dc=[l,function(a){return dU(ni,nh)}],bx=[l,function(a){return dU(nk,nj)}];function
b6(b){if(0===b){var
c=u(db),e=v===c?db[1]:l===c?a(t[2],db):db;return a(aO[50],e)}var
d=u(dc),f=[0,b6(b-1|0)],g=v===d?dc[1]:l===d?a(t[2],dc):dc,h=[0,a(aO[50],g),f];return a(R[13],h)}function
dX(d){var
b=a(R[26],d);if(9===b[0]){var
c=b[2];if(1===c.length-1)return dX(c[1])+1|0}return 0}function
dY(e,d,c){var
f=a(aA[9],c),g=a(r[77],f),h=a(j[1][10][35],g),i=b(j[1][10][7],e,h);return b(aV[26],d,i)}function
hf(d,c,b){return dY(d,c,a(y[8],b))}var
cs=a(hg[3],aC[39]),ct=[l,function(b){return a(aC[36],0)[3]}],no=[l,function(a){return g(aC[2],nn,nm,nl)}],nr=[l,function(a){return ac(nq,np)}],by=[l,function(a){return g(aC[2],nu,nt,ns)}],bz=[l,function(a){return g(aC[2],nx,nw,nv)}],ap=[l,function(a){return ac(nz,ny)}],nC=[l,function(a){return ac(nB,nA)}],nF=[l,function(a){return ac(nE,nD)}],nI=[l,function(a){return ac(nH,nG)}],nL=[l,function(a){return ac(nK,nJ)}],nO=[l,function(a){return ac(nN,nM)}],nS=[l,function(a){return g(aC[2],nR,nQ,nP)}],fm=[0,cs,ct,no,nr,0,na,m6,m9,[l,function(a){return g(aC[2],nV,nU,nT)}],nS],nZ=[l,function(a){return g(aC[2],nY,nX,nW)}],bA=[0,fm],n3=[0,nF,nI,nL,nO,2,nC,ah,bh,[l,function(a){return g(aC[2],n2,n1,n0)}],nZ];function
hh(a){bA[1]=a;return 0}function
cu(a){return bA[1][5]}function
hi(d){var
b=bA[1][1],c=u(b);return v===c?b[1]:l===c?a(t[2],b):b}function
hj(d){var
b=bA[1][2],c=u(b);return v===c?b[1]:l===c?a(t[2],b):b}function
hk(d){var
b=bA[1][3],c=u(b);return v===c?b[1]:l===c?a(t[2],b):b}function
hl(d){var
b=bA[1][4],c=u(b);return v===c?b[1]:l===c?a(t[2],b):b}function
fn(d){var
b=bA[1][7],c=u(b);return v===c?b[1]:l===c?a(t[2],b):b}function
hm(d){var
b=bA[1][8],c=u(b);return v===c?b[1]:l===c?a(t[2],b):b}function
hn(d){var
b=bA[1][6],c=u(b);return v===c?b[1]:l===c?a(t[2],b):b}function
ho(b){var
e=cu(0),f=b[1],g=a(A[2],0),d=L(w[l4],0,0,g,f,e),h=d[2];b[1]=d[1];return a(c[13],h)}function
b7(g,b,f,e){var
d=c0(c[b5],0,0,0,g,b[1],f),h=d[2];b[1]=d[1];return a(c[21],[0,h,e])}function
cv(d,a,c){var
b=aF(dZ[5],0,0,0,n4,d,a[1],c),e=b[2];a[1]=b[1];return e}function
bi(b,a,e,d,c){var
f=[0,cv(b,a,e),d,c];return b7(b,a,hi(0),f)}function
d0(b,a,d,c){var
e=[0,cv(b,a,d),c];return b7(b,a,hj(0),e)}function
fo(c,b,h,g,f,e){var
i=cv(c,b,f),d=u(by),j=[0,cv(c,b,h),g,i,e],k=v===d?by[1]:l===d?a(t[2],by):by;return b7(c,b,k,j)}function
fp(c,b,f,e){var
d=u(bz),g=[0,cv(c,b,f),e],h=v===d?bz[1]:l===d?a(t[2],bz):bz;return b7(c,b,h,g)}var
av=0;function
aG(d,c){var
e=[0,a(aN[31],d)],f=[29,[0,av,[3,[0,av,[0,b(S[1],0,e),c]]]]];return a(M[13][26],f)}function
dd(d,c){var
e=b(ad[11],d,c);return a(H[7],e)[2][1]}function
hp(b){var
a=[0,b],c=as(n7,n6,a),d=dd(a[1],c);return[0,a[1],d]}function
hq(b){var
a=[0,b],c=as(n9,n8,a),d=dd(a[1],c);return[0,a[1],d]}function
hr(a){var
b=as(n$,n_,a);return dd(a[1],b)}function
hs(a){return as(oc,ob,a)}function
ht(a){return as(oe,od,a)}function
hu(a){return as(og,of,a)}function
hv(a){return as(oi,oh,a)}function
ol(a){return as(ok,oj,a)}function
on(a){return as(d1,om,a)}function
op(a){return as(d1,oo,a)}function
or(a){return as(d1,oq,a)}var
de=[l,function(a){return ac(ot,os)}],bB=[l,function(a){return ac(ov,ou)}],oy=[l,function(a){return ac(ox,ow)}],bC=[l,function(a){return ac(oA,oz)}],df=[l,function(a){return ac(oC,oB)}],bD=[l,function(a){return ac(oE,oD)}],fq=a(j[1][6],oF);function
hw(a){return as(oH,oG,a)}function
hx(a){return as(oJ,oI,a)}var
bE=[l,function(a){return ac(oL,oK)}];function
oO(a){return as(oN,oM,a)}function
hy(a){return as(oQ,oP,a)}var
dg=[l,function(f){var
b=u(bD),c=0,d=v===b?bD[1]:l===b?a(t[2],bD):bD,e=[0,[0,0,[1,a(au[8],d)]],c];return a(s[68],e)}],ae=[l,function(a){return ac(oS,oR)}],aw=[l,function(a){return ac(oU,oT)}];function
hz(d,c){var
e=ac(d,c),f=a(au[8],e);return b(j[x][1],f,0)}var
aD=[l,function(a){return hz(oW,oV)}],ai=[l,function(a){return hz(oY,oX)}];function
oZ(e,g){var
a=g;for(;;){var
h=b(r[57],e,a),f=b(r[60],e,h),d=b(c[3],e,f);switch(d[0]){case
6:var
a=d[3];continue;case
8:var
a=d[4];continue;case
9:var
a=d[1];continue;default:return f}}}function
$(a){var
b=a[3],c=a[2],d=a[1];return c?[1,d,c[1],b]:[0,d,b]}function
d2(e,h,d){function
j(e,d){if(d){var
k=d[2],f=a(C[1][1][17],d[1]),l=f[3],m=f[2],n=f[1],o=j(e-1|0,k),p=g(c[i][2],h,e,l),q=b(c[i][2],h,e);return[0,$([0,n,b(H[16],q,m),p]),o]}return 0}return j(a(C[1][4],d)+e|0,d)}function
a6(b,a){return d2(0,b,a)}function
hA(d){var
f=a(c[i][1],1);return b(e[17][15],f,d)}function
fs(q,i,d){var
r=[0,j[1][10][1],j[19][1]];function
x(c,d){var
i=c[2],k=c[1];try{var
q=a(aI[15],d),e=q}catch(c){c=G(c);if(c!==X)throw c;var
l=a(f[3],d),m=a(f[3],o1),n=b(f[12],m,l),e=g(af[6],0,o2,n)}var
h=a(aI[14][18],e),o=h[1],p=b(j[19][7],h[2],i);return[0,b(j[1][10][7],o,k),p]}var
o=g(e[17][18],x,r,q),z=i?b(y[19],d,i[1][1]):a(y[7],d),l=a(y[2],d),t=a(y[8],d),u=o[2],v=o[1];function
h(f){var
d=b(c[3],l,f);switch(d[0]){case
1:var
k=d[1];if(b(j[1][10][3],k,v)){var
m=b(aA[38],k,t);return m?[0,1,a(c[8],m[1])]:[0,0,f]}break;case
9:var
n=d[2],o=d[1],p=h(o);if(0===p[1]){var
z=function(f,c,a){var
b=c[2],d=c[1];if(d)return[0,d,[0,a,b]];var
e=h(a);return 0===e[1]?[0,0,[0,a,b]]:[0,1,[0,e[2],b]]},q=g(e[19][46],z,o0,n),A=q[2];if(q[1]){var
B=a(e[17][9],A),C=[0,o,a(e[19][12],B)];return[0,1,a(c[21],C)]}return[0,0,f]}var
D=a(c[21],[0,p[2],n]);return[0,1,b(U[25],w[16],D)];case
10:var
r=d[1],s=r[1],E=r[2];if(b(j[19][3],s,u)){var
F=[0,s,b(c[2][2],l,E)],G=b(aA[55],t,F);return[0,1,a(c[8],G)]}break}var
i=[0,0];function
x(a){if(i[1])return a;var
b=h(a),c=b[2];i[1]=b[1];return c}var
y=g(c[100],l,x,f);return[0,i[1],y]}var
n=h(z),p=n[2];if(n[1]){if(i){var
A=i[1],B=a(s[47],p),C=g(s[54],0,B,A);return b(k[70][8],C,d)}var
D=b(s[5],p,2);return b(k[70][8],D,d)}var
E=a(f[3],o3);return g(m[24],0,E,d)}function
hB(d){var
c=d;for(;;){var
b=a(R[26],c);switch(b[0]){case
9:var
c=b[1];continue;case
10:var
e=a(j[17][9],b[1][1]);return a(j[6][5],e);default:throw[0,P,o4]}}}var
hC=a(e[17][15],hB),o6=b(e[17][15],j[1][6],o5),hD=a(j[5][4],o6);function
o7(b){var
c=a(j[6][4],b),d=a(j[5][4],0);return g(j[13][1],[0,hD],d,c)}function
hE(c){var
d=b(S[1],0,[1,[0,c]]),e=a(n[4],D[7]);return[0,b(n[7],e,d)]}function
hF(d,c){var
e=[0,hE(c),[0,[1,[0,d]],0]],f=[0,a(aN[31],o8)];return[29,[0,av,[3,[0,av,[0,b(S[1],0,f),e]]]]]}function
hG(e,d,c){var
f=[0,hE(d),[0,[1,[0,e]],[0,[1,[0,c]],0]]],g=[0,a(aN[31],o9)];return[29,[0,av,[3,[0,av,[0,b(S[1],0,g),f]]]]]}function
hH(a){return aG(o_,0)}function
hI(a){return aG(o$,0)}function
dh(a){return aG(pa,0)}function
hJ(a){return aG(pb,0)}function
hK(a){return aG(pc,0)}function
hL(a){return aG(pd,0)}function
hM(a){return aG(pe,0)}function
hN(a){return aG(pf,0)}function
hO(a){return aG(pg,0)}function
hP(a){return aG(ph,0)}function
d3(a){return[2,b(S[1],0,[1,a])]}var
pk=b(e[17][15],j[1][6],pj),pl=[0,a(j[5][4],pk)],pn=a(j[6][4],pm),po=g(j[13][1],pl,j[5][6],pn);function
hQ(p){var
d=a(j[1][6],pi),e=a(n[6],D[13]),f=a(Y[3],e),h=a(aO[35],[0,p,aP[29][1]]),i=a(c[8],h),k=b(Y[1][8],f,i),l=Y[5][1],m=[0,g(j[1][11][4],d,k,j[1][11][1]),l],o=[29,[0,av,[3,[0,av,[0,[0,[0,av,po]],[0,[2,[1,b(S[1],0,d)]],0]]]]]];return b(M[13][23],m,o)}function
pp(d){var
c=[0,b(ao[43],j[1][10][1],d)],e=[0,[2,b(S[1],0,c)],0],f=[0,a(aN[31],pq)],g=[29,[0,av,[3,[0,av,[0,b(S[1],0,f),e]]]]],h=a(M[9][3],g),i=a(n[5],M[2][1]);return b(n[7],i,h)}function
hR(a){return aG(pr,[0,d3(a),0])}function
ps(a){return aG(pt,[0,d3(a),0])}function
hS(a){return aG(pu,[0,d3(a),0])}function
ft(a){return aG(pv,0)}function
pw(a){return aG(px,[0,d3(a),0])}function
py(d,c,b){return b7(d,c,a(aC[51],0),[0,b])}function
pz(d,e){var
f=a(C[1][1][2],d);if(f)return b(c[i][5],f[1],e);var
g=a(C[1][1][3],d),h=[0,a(C[1][1][1],d),g,e];return a(c[18],h)}function
hT(f,e,d){var
h=a(c[9],1);return g(r[37],f,h,d)?b(c[35],e,d):b(c[i][5],c[14],d)}function
hU(c,b,a){function
d(b,a){return hT(c,a,b)}return g(e[17][18],d,b,a)}function
pA(d,e){var
f=a(C[1][1][2],d);if(f)return b(c[i][5],f[1],e);var
g=a(C[1][1][3],d),h=[0,a(C[1][1][1],d),g,e];return a(c[19],h)}function
hV(j,h,d){var
e=a(C[1][1][17],h),f=e[2],k=e[3],l=e[1];if(f)return b(c[i][5],f[1],d);var
m=a(c[9],1);return g(r[37],j,m,d)?a(c[19],[0,l,k,d]):b(c[i][5],c[14],d)}function
hW(j,h,d){var
e=a(C[1][1][17],h),f=e[2],k=e[3],l=e[1];if(f)return b(c[i][5],f[1],d);var
m=a(c[9],1);return g(r[37],j,m,d)?a(c[18],[0,l,k,d]):b(c[i][5],c[14],d)}function
b8(h,a,f,d){function
i(e,d){var
f=b(c[35],d,e);return b(U[29],a,f)}var
j=g(e[17][18],i,f,d);return g(U[17],h,a,j)}function
fu(j,d,h,f){function
k(f,e){var
g=0===a(C[1][1][1],e)?b(c[i][5],c[14],f):b(c[35],e,f);return b(U[29],d,g)}var
l=g(e[17][18],k,h,f);return g(U[17],j,d,l)}function
d4(d,a){function
f(d,a){return b(c[36],a,d)}var
h=g(e[17][18],f,d,a);return b(U[29],w[16],h)}function
pB(c,b,a){function
d(b,a){return hV(c,a,b)}return g(e[17][18],d,b,a)}function
pC(c,b,a){function
d(b,a){return hW(c,a,b)}return g(e[17][18],d,b,a)}function
fv(f,d){var
g=a(c[i][1],f);return b(e[17][15],g,d)}function
cx(d,g,i,h){var
m=g?g[1]:0;function
f(g,i){var
h=b(c[3],d,i);switch(h[0]){case
1:return b(j[1][10][4],h[1],g);case
9:var
n=h[2],k=b(c[3],d,h[1]);switch(k[0]){case
11:var
l=k[1][1];break;case
12:var
l=k[1][1][1];break;default:return o(c[lI],d,f,g,i)}var
p=a(A[27],l)[1],q=m?0:p[6];return o(e[19][51],q,f,g,n);default:return o(c[lI],d,f,g,i)}}return f(i,h)}function
d5(j,d,g){var
f=b(c[3],j,d);switch(f[0]){case
11:var
h=f[1][1];break;case
12:var
h=f[1][1][1];break;default:return[0,d,g]}var
k=a(A[27],h)[1][7],i=b(e[19][54],k,g),l=i[2];return[0,a(c[21],[0,d,i[1]]),l]}function
hX(f,a,e,d){try{var
b=aF(U[83],0,pD,0,f,a[1],e,d),c=b[2],g=b[1],h=c?(a[1]=g,1):c;return h}catch(a){a=G(a);if(a===hY[6])return 0;throw a}}function
pE(h,f,d){var
e=j[1][10][1];function
i(t,k,i){var
e=a(C[2][1][17],k),l=e[3],m=e[2],n=e[1],p=0;function
q(b){var
e=a(c[8],b);return o(r[33],d,h,f,e)}if(!g(H[24],q,p,m)){var
s=a(c[8],l);if(!o(r[33],d,h,f,s))return i}return b(j[1][10][4],n,i)}return g(aA[39],i,d,e)}var
pF=j[1][10][1];function
pG(c,a){return b(j[1][10][4],a,c)}var
pH=b(e[17][18],pG,pF);function
hZ(a){return b(M[5][10],aN[41],a)}function
h0(c){var
b=c[1];return 0===b[0]?a(aN[40],b[1]):b[1][1]}function
d6(b){var
c=h0(b);return a(j[1][6],c)}var
pI=ax[2];function
pJ(a){return g(pI,0,0,a)}var
cy=a(y[24],pJ);function
h1(c,m){function
d(d){var
h=a(k[66][6],d),n=a(k[66][4],d),o=b(r[42],h,m),p=b(y[42][16],c,d),q=b(r[42],h,p),t=b(j[1][10][9],o,q);function
u(c){var
d=a(C[2][1][1],c);return b(j[1][10][3],d,t)}var
v=a(e[17][9],n),i=b(e[17][aY],u,v)[2];if(i)var
l=a(C[2][1][1],i[1]);else
var
w=a(f[3],pK),x=a(j[1][9],c),z=a(f[3],pL),A=b(f[12],z,x),B=b(f[12],A,w),l=g(af[6],0,pM,B);return b(s[81],c,[0,l])}return a(k[66][10],d)}function
aj(e,c){return bf[1]?function(h){var
d=a(bj[84],h),i=a(f[3],pN),j=a(f[3],e),m=a(f[3],pO),n=b(f[12],m,j),o=b(f[12],n,i),p=b(f[12],o,d);b(at[10],0,p);function
q(i){var
c=i[1];if(c[1]===cz[29])var
d=c[3],o=c[2],p=a(bj[84],h),j=u(d),q=a(f[3],pP),r=v===j?d[1]:l===j?a(t[2],d):d,s=a(f[13],0),w=a(f[3],e),x=a(f[3],pQ),y=a(f[16],o),z=a(f[3],pR),A=b(f[12],z,y),B=b(f[12],A,x),C=b(f[12],B,w),D=b(f[12],C,s),E=b(f[12],D,r),F=b(f[12],E,q),m=b(f[12],F,p);else{if(c[1]===h2[1])var
I=g(h3[2],c[2],c[3],c[4]),J=a(f[3],pT),n=b(f[12],J,I);else
var
n=a(af[17],i);var
m=n}var
G=a(f[3],pS),H=b(f[12],G,m);b(at[10],0,H);return a(k[16],0)}function
r(c){if(0===c){var
d=a(f[3],pU),g=a(f[3],e),h=b(f[12],g,d);b(at[10],0,h);return a(k[16],0)}return aB(function(c){var
d=a(bj[84],c),e=a(f[3],pV),g=b(f[12],e,d);b(at[10],0,g);return[0,[0,c[1],0],c[2]]})}var
s=b(k[71][1],k[53],r),w=aB(c),x=b(k[18],w,s);return a(z(b(k[23],x,q)),h)}:c}function
V(b,a){return g(C[1][14],c[9],b,a)}function
aQ(b,a){return g(C[1][13],c[9],b,a)}var
ak=C[1][1][17],bF=C[2][1][17],pW=C[2][1][18];function
h4(a){return b(e[17][15],$,a)}function
pX(a){return[0,a]}function
pY(a){return[1,a]}var
bG=C[1][1][3],fw=C[1][1][2],b9=C[1][1][1],fx=C[2][1][3],fy=C[2][1][2];function
pZ(b,a){return[0,b,a]}function
ay(c,b,a){return b?[1,c,b[1],a]:[0,c,a]}function
fz(c,b,a){return b?[1,c,b[1],a]:[0,c,a]}var
p0=C[1][7];function
cA(f,m,h){var
n=f?f[1]:0;function
j(o,d){var
h=d[2],j=d[1],p=d[4],q=d[3],r=a(c[i][4],j),e=b(C[1][1][14],r,o),k=a(b9,e),f=k?k[1]:a(m,0),s=a(bG,e),t=[0,f,a(fw,e),s],u=a(C[2][1][18],t);if(n)var
g=0;else
if(a(C[1][1][6],e))var
g=0;else
var
l=h,g=1;if(!g)var
l=[0,a(c[10],f),h];return[0,[0,a(c[10],f),j],l,[0,f,q],[0,u,p]]}var
d=g(e[17][19],j,h,p1),k=d[4],l=d[1];return[0,l,a(e[17][9],d[2]),k]}function
cB(d){function
f(h,f){var
d=f[2],j=f[1],e=a(bF,h),g=e[1],k=e[2],l=b(c[i][11],d,e[3]),m=a(c[i][11],d);return[0,[0,ay([0,g],b(H[16],m,k),l),j],[0,g,d]]}return g(e[17][19],f,d,p2)}var
d7=aI[4];function
h5(c,b){if(0===b[0]){var
d=b[1];return[0,d,a(c,b[2])]}var
e=b[2],f=b[1],g=a(c,b[3]);return[1,f,a(c,e),g]}function
d8(d,f,a){var
h=[0,d,0];function
j(e,a){var
d=a[1],g=a[2];return[0,d+1|0,[0,h5(b(c[i][3],f,d),e),g]]}return g(e[17][19],j,a,h)[2]}function
di(f,d){function
h(a,e){var
d=a[1],g=a[2];return[0,d+1|0,[0,h5(b(c[i][3],[0,f,0],d),e),g]]}var
j=g(e[17][18],h,p3,d)[2];return a(e[17][9],j)}function
d9(l,k,j){var
f=1,g=0,d=j;for(;;){if(d){var
h=d[2],m=d[1];if(f===l){var
n=a(e[17][9],g),o=d8(0,[0,b(c[i][1],-f|0,k),0],n);return b(e[18],o,h)}var
f=f+1|0,g=[0,m,g],d=h;continue}return 0}}function
h6(m,l,k){var
f=1,g=0,d=k;for(;;){if(d){var
j=d[2],h=d[1];if(f===m){var
n=a(C[1][1][3],h),o=b(c[i][1],-f|0,l),p=[0,[1,a(C[1][1][1],h),o,n],j],q=a(e[17][9],g);return b(e[18],q,p)}var
f=f+1|0,g=[0,h,g],d=j;continue}return 0}}function
aZ(b){return a(C[2][1][1],b)}var
d_=C[2][9],d$=C[1][8],bb=C[1][1][14],h7=C[2][1][14],h8=C[2][7],h9=C[2][5];function
h_(g,m,l){var
f=0,d=l;for(;;){if(d){var
h=d[2],k=d[1],n=aZ(k);if(b(j[1][1],n,g)){var
o=a(e[17][9],f);return b(e[18],o,h)}var
f=[0,b(h7,a(c[i][9],[0,[0,g,m],0]),k),f],d=h;continue}return 0}}function
dj(a){return b(at[6],0,a)}function
bR(a){return g(af[6],a[1],[0,a[2]],a[3])}function
a7(b){var
c=a(f[3],b);return g(af[6],0,0,c)}function
b_(b,a){return g(af[6],0,[0,b],a)}var
h$=af[4];function
p4(a){return b(af[16],0,a)}var
ea=U[20];function
fA(b,a){return g(af[3],0,b,a)}function
ia(j,i,h,e,f){var
k=a(c[bO][1],h),d=b(w[4],j,k),l=e?[0,d[1],d[2],d[3],d[4],e[1],d[6],d[7]]:d;return g(w[22],f,i,l)}function
cC(d,c,b,a){return ck(B[4],d,c,b,0,0,0,0,0,a)}function
ib(d,c,b,a){return aF(B[8],d,c,b,0,0,0,a)}function
p5(a){return a}function
p6(a){return a}var
ic=p7[7];function
eb(c,b,a){return g(aI[22],0,[0,a,0],[4,[0,[1,c],0],b])}function
fB(e,d,a){var
f=b(c[5],e,a);return b(au[11],d,f)}function
dk(d,b){var
e=bQ(d,b),f=a(aO[35],e);return a(c[8],f)}function
ec(f,d){var
g=a(c[126],f),h=b(e[17][15],g,d),i=a(r[88],h);return b(e[17][15],c[W],i)}function
a8(d,a){var
f=b(r[8],d,a);return b(e[19][15],c[8],f)}function
aE(d,b){return a(c[34],[0,d,b])}function
fC(d,c,a){return b(ad[16],c,a)}function
cD(e,d){var
a=b(c[3],e,d);return 9===a[0]?[0,a[1],a[2]]:[0,d,[0]]}function
dl(f){var
d=a(_[6],f),g=d[1],h=b(e[17][15],c[8],d[2]);return[0,c$(g),h]}function
fD(d,g,f){var
h=a(c[5],d),i=b(e[19][15],h,f),j=b(c[5],d,g),k=b(b$[74],j,i);return a(c[8],k)}function
id(d,g,f){var
h=a(c[5],d),i=b(e[19][15],h,f),j=b(c[5],d,g),k=b(hY[25],j,i);return a(c[8],k)}function
ed(d,c,b){var
a=g(_[71],d,c,b);return[0,a[1],a[2]]}function
dm(d){var
c=a(aP[37][4],d),e=[0,c,b(aP[37][7],c,d)],f=a(aP[36][1],e);return[0,c,a(aP[40][15],f)]}aH(1043,[0,cp,dS,dT,bf,z,aB,my,mz,mA,mS,mT,hb,mU,g$,ha,oZ,cw,av,dY,hf,aG,d2,a6,hA,fv,fh,ff,g8,fg,hX,fk,py,pz,hT,hU,pA,hV,hW,b8,fu,d4,pB,pC,cx,pE,pH,d5,cv,a5,ar,fi,as,ac,dU,fj,hc,dd,dV,bw,da,hh,fm,n3,cu,hi,hj,hk,hl,fn,hm,hn,ah,bh,dW,fl,ae,aw,aD,ai,db,dc,bx,b6,dX,cs,ct,by,bz,ap,ho,b7,bi,d0,fo,fp,n5,oa,d1,fr,hp,hq,hr,hs,ht,hu,hv,ol,on,op,or,de,bB,oy,bC,df,bD,fq,hw,hx,bE,oO,hy,dg,aj,hD,o7,hH,hI,dh,hJ,hK,hL,hM,hN,hO,hQ,pp,hR,ps,hS,ft,pw,hF,hG,fs,hP,hB,hC,hZ,h0,d6,cy,h1,V,aQ,ak,bF,$,pW,bG,b9,fw,pZ,ay,fz,h4,pX,pY,cA,cB,d8,aZ,fx,fy,p0,d_,d$,bb,h7,h8,h9,p5,p6,dj,bR,a7,b_,h$,p4,fA,ea,di,d9,h6,h_,ia,cC,ib,d7,ic,eb,c$,bQ,fB,dk,ec,a8,aE,fC,cD,dl,fD,id,ed,dm],lH);function
ie(g,f,e){var
c=a(A[2],0),h=a(w[17],c),d=b(B[13],h,e);return o(g,c,d[1],f,d[2])}function
ca(i,e,d){return ie(function(l,d,k,j){var
e=b(c[3],d,j);if(11===e[0]){var
h=e[1];return o(i,l,d,k,[0,h[1],h[2]])}var
m=a(f[3],p8);return g(af[6],0,0,m)},e,d)}var
fF=[0,fE[52][1]];function
cb(a){fF[1]=g(fE[52][4],a[1],a[2],fF[1]);return 0}function
p9(d){try{var
c=b(fE[52][22],d,fF[1]);return c}catch(c){c=G(c);if(c===X){var
e=a(f[3],d),h=a(f[3],p_),i=b(f[12],h,e);return g(af[6],0,0,i)}throw c}}function
ig(d,c,a){function
e(a){var
c=a[2];b(dn[12],a[1],c);return c}var
f=b(p[17],e,a);function
g(e){var
a=p9(e);function
c(c){return b(a,d,c)}return b(p[15],c,f)}return b(p[15],g,c)}aH(1047,[0,ie,ca,cb,ig],dP);function
ih(d,i){var
c=i[2];switch(c[0]){case
0:var
k=c[1],l=c[2]?a(f[3],p$):a(f[7],0),m=a(j[1][9],k);return b(f[12],m,l);case
1:var
g=c[3],h=b(bj[62],d,c[1]);if(a(e[17][55],g))return h;var
n=a(f[3],qa),o=cE(d,g),p=a(f[13],0),q=a(f[3],qb),r=b(f[12],q,h),s=b(f[12],r,p),t=b(f[12],s,o);return b(f[12],t,n);default:var
u=c[1],v=a(f[3],qc),w=a(bS[20],u),x=a(f[3],qd),y=b(f[12],x,w);return b(f[12],y,v)}}function
cE(b,a){function
c(a){return ih(b,a)}return g(f[39],f[13],c,a)}function
qe(b){return dj(cE(a(A[2],0),b))}function
ii(d,c){switch(c[0]){case
0:var
h=c[2],i=c[1],aY=function(e){var
c=e[1],g=e[2],h=a(cc(d),g),i=c[3],k=c[1][2],l=a(bS[20],c[4]),m=a(f[3],qv),n=a(bS[17],i),o=a(j[1][9],k),p=b(f[12],o,n),q=b(f[12],p,m),r=b(f[12],q,l);return b(f[12],r,h)},aZ=g(f[39],f[5],aY,h),k=a(bS[20],i),l=a(f[13],0),m=a(f[3],qf),n=a(f[13],0),o=b(f[12],n,m),p=b(f[12],o,l),q=b(f[12],p,k);return b(f[12],q,aZ);case
1:var
r=a(j[1][9],c[1][2]),s=a(f[13],0),t=a(f[3],qg),u=a(f[13],0),v=b(f[12],u,t),w=b(f[12],v,s);return b(f[12],w,r);case
2:var
x=c[4],y=c[3],z=c[1],A=a(f[3],qh),B=a(cc(d),x),C=a(f[3],qi),D=b(f[12],C,B),E=b(f[12],D,A),F=b(f[26],1,E),G=a(f[13],0),H=function(b){return a(j[1][9],b[2])},I=b(f[34],H,y),J=a(f[13],0),K=a(bS[20],z),L=a(f[3],qj),N=a(f[13],0),O=a(f[3],qk),P=a(f[13],0),Q=b(f[12],P,O),R=b(f[12],Q,N),S=b(f[12],R,L),T=b(f[12],S,K),U=b(f[12],T,J),V=b(f[12],U,I),W=b(f[12],V,G);return b(f[12],W,F);case
3:var
X=c[2],Y=c[1],Z=a(f[3],ql),_=a(cc(d),X),$=a(f[3],qm),aa=b(f[12],$,_),ab=b(f[12],aa,Z),ac=b(f[26],1,ab),ad=a(f[13],0),ae=a(f[3],qn),af=a(f[13],0),ag=a(bS[20],Y),ah=a(f[13],0),ai=a(f[3],qo),aj=a(f[13],0),ak=b(f[12],aj,ai),al=b(f[12],ak,ah),am=b(f[12],al,ag),an=b(f[12],am,af),ao=b(f[12],an,ae),ap=b(f[12],ao,ad);return b(f[12],ap,ac);default:var
e=c[1];if(0===e[0]){var
aq=c[2],ar=e[1],as=a(f[3],qp),at=a(cc(d),aq),au=a(f[3],qq),av=b(f[12],au,at),aw=b(f[12],av,as),ax=b(f[26],1,aw),ay=a(f[13],0),az=a(M[5][23],ar),aA=a(f[13],0),aB=a(f[3],qr),aC=a(f[13],0),aD=b(f[12],aC,aB),aE=b(f[12],aD,aA),aF=b(f[12],aE,az),aG=b(f[12],aF,ay);return b(f[12],aG,ax)}var
aH=c[2],aI=e[1],aJ=a(f[3],qs),aK=a(cc(d),aH),aL=a(f[3],qt),aM=b(f[12],aL,aK),aN=b(f[12],aM,aJ),aO=b(f[26],1,aN),aP=a(f[13],0),aQ=b(M[5][25],d,aI),aR=a(f[13],0),aS=a(f[3],qu),aT=a(f[13],0),aU=b(f[12],aT,aS),aV=b(f[12],aU,aR),aW=b(f[12],aV,aQ),aX=b(f[12],aW,aP);return b(f[12],aX,aO)}}function
dp(c,a){var
d=a[2],e=ii(c,a[3]),g=cE(c,d);return b(f[12],g,e)}function
cc(a){function
c(b){return dp(a,b)}return b(f[39],f[5],c)}function
qw(b){return dj(dp(a(A[2],0),b))}function
ee(d,a){var
c=b(aV[25],d,a[1]);a[1]=b(j[1][10][4],c,a[1]);return c}function
ef(e,d,c,b){return a(f[7],0)}function
eg(e,d,c,b){return a(f[7],0)}function
ij(a){if(a)if(0===a[1][0])return 1;return 0}function
eh(g,d,f){if(0===d[0]){var
h=d[1][2],e=b(c[3],g,f);switch(e[0]){case
1:return b(j[1][1],h,e[1]);case
10:var
i=a(j[17][9],e[1][1]),k=a(j[6][7],i);return b(j[1][1],h,k);default:return 0}}return fB(g,[1,d[1][3]],f)}var
ik=a(J[3],qx);function
fG(m,h,d,c){if(0===c[0]){var
i=c[1];if(i)return[0,d,[0,i[1],0]];var
k=ee(a(j[1][6],qy),h);h[1]=b(j[1][10][4],k,h[1]);return[0,d,[0,k,1]]}var
l=c[1],n=c[2],o=l[1];if(c[3]){var
p=a(f[3],qz);return g(af[6],d,qA,p)}function
q(a,b){return fG(m,h,a,b)}var
r=a(il[9],q),s=b(e[17][15],r,n);return[0,d,[1,l,a(_[35],o),s]]}function
fH(a){var
c=j[1][10][1];function
d(a,f){var
c=f[2];if(typeof
c==="number")return a;else
switch(c[0]){case
0:var
g=c[1][2],d=fH(c[2]);try{var
h=d6(g),i=b(j[1][10][4],h,d),e=i}catch(a){var
e=d}return b(j[1][10][7],e,a);case
1:return a;default:return a}}return g(e[17][18],d,c,a)}function
ei(n,s,r){var
c=r[2],o=r[1],h=s?s[1]:[0,j[1][10][1]];if(typeof
c==="number"){var
t=ee(a(j[1][6],qD),h);h[1]=b(j[1][10][4],t,h[1]);return[0,[0,o],[0,t,1]]}else
switch(c[0]){case
0:var
i=c[2],u=c[1],k=u[2],d=u[1];try{var
J=[0,b(cr[5],0,k)],l=J}catch(a){var
l=[1,[0,d6(k),0]]}if(0===l[0]){var
v=l[1];if(3===v[0]){var
m=v[1],F=a(_[35],m[1]),w=a(_[47],m),x=a(e[17][1],i);if(x<w)var
G=b(e[17][66],w-x|0,[0,d,0]),y=b(e[18],G,i);else
var
y=i;b(dn[12],[0,d],[3,m]);var
H=[0,h],I=function(a){return ei(n,H,a)};return[0,[0,d],[1,m,F,b(e[17][15],I,y)]]}if(0===i)return[0,[0,d],[0,d6(k),0]];var
A=a(f[3],qE),B=hZ(k),C=a(f[3],qF),D=b(f[12],C,B),E=b(f[12],D,A);return g(af[6],[0,d],qG,E)}return[0,[0,d],l[1]];case
1:return[0,[0,o],[2,c[1]]];default:var
p=b(a0[8],n,c[1])[2];if(p)if(p[2])var
q=0;else
var
K=p[1][2],L=function(a,b){return fG(n,h,a,b)},z=b(il[9],L,K),q=1;else
var
q=0;if(!q)var
z=bR([0,[0,o],qI,a(f[3],qH)]);return z}}function
im(w,l,k,B,c){var
d=[0,j[1][10][1]],n=[0,d];function
C(a){return ei(k,n,a)}function
h(p,h,i,D,n){var
q=n[3],r=n[2],s=n[1];if(0===r[0])var
c=r[1];else
var
Q=r[1],R=function(a){return[0,0,a]},T=b(e[17][15],R,Q),c=b(e[18],D,T);function
E(a){return a[2]}var
F=fH(b(e[17][15],E,c));d[1]=b(j[1][10][7],d[1],F);function
I(c){var
d=c[2],e=c[1];if(1-b(j[1][1],d,h)){var
g=a(j[1][9],h),i=a(f[3],qL);bR([0,[0,e],qM,b(f[12],i,g)])}var
k=a(j[1][8],d);return o(dn[17],[0,e],qO,k,qN)}b(H[13],I,s);function
k(f,c){if(f){var
g=f[2],h=f[1];if(a(cd[14],h)){var
i=a(cd[16],h),l=function(c){var
a=c[1];return a?b(j[1][1],a[1][2],i):0};try{var
p=function(a){return l(a)?[0,a[2]]:0},q=b(e[17][c3],p,c),r=[0,q,k(g,b(e[17][99],l,c))];return r}catch(a){a=G(a);if(a===X){var
m=ee(i,d),n=[2,[0,b(S[1],0,[1,m])]],o=[2,b(S[1],0,n)];d[1]=b(j[1][10][4],m,d[1]);return[0,[0,ik,o],k(g,c)]}throw a}}if(c){var
s=c[1],t=k(g,c[2]);return[0,s[2],t]}var
u=function(a){return a[2]};return b(e[17][15],u,c)}function
v(a){return a[2]}return b(e[17][15],v,c)}var
A=k(B,c);if(s)var
x=s[1][1];else
var
y=a(e[17][5],c),z=y[1],P=z?z[1][1]:y[2][1],x=P;var
K=a(e[17][mx],c)[2][1],t=b(J[4],x,K),l=b(e[17][15],C,A);function
v(d,c){function
h(c,e){var
d=e[2],i=e[1];switch(d[0]){case
0:var
h=d[1];if(b(j[1][10][3],h,c)){var
k=a(f[3],qJ);return g(af[6],i,qK,k)}return b(j[1][10][4],h,c);case
1:return v(c,d[3]);default:return c}}return g(e[17][18],h,d,c)}v(j[1][10][1],l);if(i){var
u=i[1];if(0===u[0]){var
L=u[1],M=function(a){var
c=a[2],d=a[1];if(1===c[0])if(!c[1])if(b(j[1][1],d,w))return 0;return[0,[0,0,[0,d,0]]]},N=b(e[17][72],M,L),O=m(p,h,i,c,q);return[0,t,b(e[18],N,l),O]}return[0,t,l,m([0,[0,h,u[1]],p],h,i,c,q)]}return[0,t,l,m(p,h,i,c,q)]}function
m(f,g,l,k,c){switch(c[0]){case
0:var
n=c[2],p=c[1],I=function(c){var
d=c[1],g=d[1],i=g[2],k=c[2],l=g[1],m=a(j[1][8],i);o(dn[17],[0,l],qQ,m,qP);var
n=0,p=0;function
q(a){return h(f,i,p,n,a)}return[0,d,b(e[17][15],q,k)]},J=b(e[17][15],I,n),q=d[1],r=function(a,b){return i(f,q,a,b)};return[0,b(S[6],r,p),J];case
1:return[1,c[1]];case
2:var
m=c[1],s=c[4],t=c[3],u=c[2],v=a(cF[6],m),w=[0,[0,g,[0,[0,a(H[7],v),g]]],f],x=function(a){return h(w,g,l,k,a)};return[2,m,u,t,b(e[17][15],x,s)];case
3:var
y=c[2],z=c[1],A=function(a){return h(f,g,l,k,a)},B=b(e[17][15],A,y),C=d[1],D=function(a,b){return i(f,C,a,b)};return[3,b(S[6],D,z),B];default:var
E=c[2],F=c[1],G=function(a){return h(f,g,l,k,a)};return[4,F,b(e[17][15],G,E)]}}function
i(f,p,n,d){var
c=n?n[1]:ik;if(7===d[0]){var
q=d[1];if(!q[1]){var
k=q[2][1];if(0===k[0]){var
r=k[1],s=r[1];if(0!==s[0]){var
l=r[2],h=s[1],A=d[2],B=k[2];if(g(e[17][137],j[1][1],h,f)){var
t=g(e[17][135],j[1][1],h,f),C=function(a){var
c=a[2],d=a[1];function
e(a,b){return i(f,p,a,b)}return[0,b(S[6],e,d),c]},u=b(e[17][15],C,A),D=[0,b(S[1],l,[1,h]),B],E=[7,[0,0,b(S[1],[0,c],D)],u],m=a(qB[18],h),x=[12,[0,[0,[1,b(j[17][1],m,m)],qC,0]],0,0],F=[0,[0,b(S[1],[0,c],x),0],0],G=[7,[0,0,b(S[1],[0,c],E)],F],v=b(S[1],[0,c],G);if(0===t[0])return v;var
w=t[1],I=a(H[3],w[1])?[0,[0,v,0],0]:0,J=b(ao[43],j[1][10][1],[1,w[3]]),K=b(e[18],u,I),L=[0,b(S[1],l,[0,J]),0],M=[7,[0,0,b(S[1],l,L)],K];return b(S[1],[0,c],M)}}}}}var
y=b(S[1],[0,c],d);function
z(b){function
c(a,c){return i(f,b,a,c)}return a(S[6],c)}return o(cF[30],j[1][10][4],z,p,y)}return h(0,w,l,0,c)}aH(1055,[0,ih,cE,cE,qe,ii,dp,cc,qw,ij,eh,ee,ef,eg,fG,fH,ei,im],mj);function
io(b,a,d){var
e=L(ax[2],0,0,b,a[1],d),c=aF(dZ[5],[0,w[aY]],qS,0,qR,b,a[1],e),f=c[2];a[1]=c[1];return f}function
ip(f,d,b){var
e=u(bB),g=[0,io(f,d,b),b],h=v===e?bB[1]:l===e?a(t[2],bB):bB,i=[0,ar(d,h),g];return a(c[21],i)}function
iq(f,d,b){var
e=u(bC),g=[0,io(f,d,b),b],h=v===e?bC[1]:l===e?a(t[2],bC):bC,i=[0,ar(d,h),g];return a(c[21],i)}function
bc(d){switch(d[0]){case
0:return a(c[9],d[1]);case
1:var
f=d[2],g=a(c[28],d[1]),h=b(e[17][15],bc,f),i=[0,g,a(e[19][12],h)];return a(c[21],i);case
2:return d[1];default:return a(c[9],d[1])}}function
ir(g,f,d,b){switch(b[0]){case
0:return a(c[9],b[1]);case
1:var
j=b[2],k=a(c[28],b[1]),l=is(g,f,d,j),m=[0,k,a(e[19][12],l)];return a(c[21],m);case
2:var
h=b[1];if(g)try{var
n=ip(f,d,h);return n}catch(a){return h}return h;default:var
i=b[1];return g?iq(f,d,a(c[9],i)):a(c[9],i)}}function
is(f,d,c,a){function
g(a){return ir(f,d,c,a)}return b(e[17][15],g,a)}function
dq(a,e,d,c){var
f=a?a[1]:1,b=[0,d],g=ir(f,e,b,c);return[0,b[1],g]}function
it(a,e,d,c){var
f=a?a[1]:1,b=[0,d],g=is(f,e,b,c);return[0,b[1],g]}function
iv(c){var
d=O[2][1];function
f(c,f){var
d=iu(f),e=b(O[2][8],d,c);if(a(O[2][2],e))return b(O[2][7],d,c);var
g=a(O[2][26],e),h=a(E[22],g),i=b(E[17],h,qT);return a7(b(E[17],qU,i))}return g(e[17][18],f,d,c)}function
iu(b){switch(b[0]){case
0:return a(O[2][5],b[1]);case
1:return iv(b[2]);case
2:return O[2][1];default:return O[2][1]}}function
dr(a){function
c(a){return[2,a]}return b(e[17][15],c,a)}function
iw(c,a){function
d(a){return ce(c,a)}return b(e[17][15],d,a)}function
ce(d,j){var
f=b(c[3],d,j);switch(f[0]){case
0:return[0,f[1]];case
9:var
i=f[2],h=f[1];if(2===i.length-1){var
k=i[2],m=u(bB),q=v===m?bB[1]:l===m?a(t[2],bB):bB;if(g(c[a4],d,q,h))return[2,k];var
n=u(bC),r=v===n?bC[1]:l===n?a(t[2],bC):bC;if(g(c[a4],d,r,h))return[3,b(c[65],d,k)]}if(b(c[56],d,h)){var
o=b(c[77],d,h),s=a(_[35],o[1][1]),p=b(e[19][54],s,i),w=p[1],x=iw(d,a(e[19][11],p[2])),y=dr(a(e[19][11],w));return[1,o,b(e[18],y,x)]}break;case
12:return[1,f[1],0]}return[2,j]}function
ix(c,d,l,f){var
m=c?c[1]:[0,j[1][10][1]],s=[0,m];function
g(f){var
c=s?m:[0,j[1][10][1]];switch(f[0]){case
0:var
n=b(e[17][7],l,f[1]-1|0),o=a(C[1][1][1],n),g=b(aV[28],o,c[1]);c[1]=b(j[1][10][4],g,c[1]);return[0,[0,d,[0,g,0]]];case
1:var
h=f[1][1],p=f[2],i=a(_[35],h[1]);return[0,[0,d,[1,h,i,ix([0,c],d,l,b(e[17][x],i,p)[2])]]];case
2:var
q=c[1],r=a(j[1][6],qV),k=b(aV[25],r,q);c[1]=b(j[1][10][4],k,c[1]);return[0,[0,d,[0,k,1]]];default:return 0}}return b(e[17][72],g,f)}function
iy(c,d,b){var
f=b[2],g=b[1],h=c?c[1]:j[1][10][1],i=ix([0,[0,h]],d,g,f);return a(e[17][9],i)}function
iz(i,e,d){var
h=g(r[W],i,e,d);if(9===b(c[3],e,d)[0]){var
j=a(f[3],qW),k=a(f[3],qX),l=b(f[12],k,h);return b(f[12],l,j)}return h}function
iA(a,d,c){var
b=dq(0,a,d,c);return iz(a,b[1],b[2])}function
cG(i,h,n){function
k(d,l){var
c=a(ak,l),e=c[2],i=c[1],m=c[3];if(e)var
n=g(r[W],d,h,e[1]),o=a(f[13],0),p=a(f[3],qY),q=b(f[12],p,o),k=b(f[12],q,n);else
var
k=a(f[7],0);var
s=i?a(j[1][9],i[1]):a(f[3],q0),t=g(r[W],d,h,m),u=a(f[3],qZ),v=b(f[12],s,k),w=b(f[12],v,u);return b(f[12],w,t)}var
d=a(e[17][9],n);if(d)var
l=d[1],o=d[2],p=k(i,l),q=[0,b(c[bu],l,i),p],s=function(e,d){var
g=e[1],h=e[2],i=k(g,d),j=a(f[3],q1),l=b(f[12],h,j),m=b(f[12],l,i);return[0,b(c[bu],d,g),m]},m=g(e[17][18],s,q,o)[2];else
var
m=a(f[7],0);return m}function
q2(c,b,a){return dj(cG(c,b,a))}function
fI(d,c,b){var
h=a(e[17][9],b);function
i(a){return iA(d,c,a)}function
j(b){return a(f[3],q3)}return g(f[39],j,i,h)}function
az(h,g,d){var
i=d[1],l=d[3],m=d[2],n=b(c[x],i,h),j=cG(h,g,i),o=cG(h,g,l),p=a(f[3],q4),q=fI(n,g,m),r=a(f[3],q5),s=a(f[3],q6);if(a(e[17][55],i))var
k=j;else
var
y=a(f[3],q7),k=b(f[12],j,y);var
t=b(f[12],k,s),u=b(f[12],t,r),v=b(f[12],u,q),w=b(f[12],v,p);return b(f[12],w,o)}function
q8(c,b,a){return dj(az(c,b,a))}function
iB(g,f,d){var
h=d[3],j=d[1],l=d[2];fg(g,f,j);fg(g,f,h);var
k=b(c[x],j,g),m=[0,f,0];function
n(l,j,d){var
e=d[2],m=d[1],n=a(ak,l)[3],f=dq(q9,k,m,j),g=f[2],h=f[1];ff(k,h,g,b(c[i][4],e,n));return[0,h,[0,g,e]]}o(e[17][24],n,h,l,m);return 0}function
ds(c,h,e,d){var
k=c?c[1]:0;if(k)return d;try{iB(h,e,d);return d}catch(c){c=G(c);if(c[1]===h2[1]){var
i=c[4];if(16===i[0]){var
j=c[2],t=g(h3[1],j,e,i[1]),u=a(f[13],0),v=az(j,e,d),w=a(f[3],rb),x=b(f[12],w,v),y=b(f[12],x,u);return b_(rc,b(f[12],y,t))}}else
if(c[1]===bd){var
z=a(f[3],c[2]),A=a(f[3],rd),B=a(f[13],0),C=az(h,e,d),D=a(f[3],re),E=b(f[12],D,C),F=b(f[12],E,B),H=b(f[12],F,A);return b_(rf,b(f[12],H,z))}if(a(h$,c)){var
l=b(af[16],0,c),m=a(f[3],q_),n=a(f[13],0),o=az(h,e,d),p=a(f[3],q$),q=b(f[12],p,o),r=b(f[12],q,n),s=b(f[12],r,m);return b_(ra,b(f[12],s,l))}throw c}}function
aR(a,f,e,d,c,b){var
g=a?a[1]:0;return ds([0,g],f,e,[0,d,c,b])}function
iC(f,d){function
g(d){switch(d[0]){case
1:var
e=d[2],g=a(f,a(c[28],d[1])),h=b(c[77],w[16],g);return[1,h,iC(f,e)];case
2:return[2,a(f,d[1])];default:return d}}return b(e[17][15],g,d)}function
a9(c,a){var
d=a[2],e=a[1],f=b(d$,c,a[3]),g=iC(c,d);return[0,b(d$,c,e),g,f]}function
ej(f,d,k,a){function
g(d,a){var
h=b(c[3],f,a);if(0===h[0]){var
j=h[1]-d|0;if(0<j)try{var
l=bc(b(e[17][7],k,j-1|0)),m=b(c[i][1],d,l);return m}catch(b){b=G(b);if(b[1]===fJ)return a;throw b}return a}function
n(a){return a+1|0}return L(c[fd],f,n,g,d,a)}return g(d,a)}function
rg(f,d,a){function
c(e,a){var
c=a[1],g=a[2];return[0,c+1|0,[0,b(bb,function(a){return ej(f,c,d,a)},e),g]]}return g(e[17][19],c,a,rh)[2]}function
ek(g,d,c){switch(c[0]){case
0:var
h=c[1];try{var
i=b(e[17][7],d,h-1|0);return i}catch(a){a=G(a);if(a[1]===fJ)return c;throw a}case
1:var
j=c[2],k=c[1];return[1,k,a(el(g,d),j)];case
2:return[2,bk(g,d,c[1])];default:var
f=b(e[17][7],d,c[1]-1|0);switch(f[0]){case
0:return[3,f[1]];case
1:throw[0,P,ri];case
2:return[2,f[1]];default:return[3,f[1]]}}}function
bk(c,b,a){return ej(c,0,b,a)}function
el(c,b){function
d(a){return ek(c,b,a)}return a(e[17][15],d)}function
cH(f,d,a){function
c(e,a){var
c=a[1],g=a[2];return[0,c+1|0,[0,b(bb,function(a){return ej(f,c,d,a)},e),g]]}return g(e[17][19],c,a,rj)[2]}function
Q(d,c,b){return bk(d,a(e[8],c),b)}function
fK(j,e,f,d){switch(d[0]){case
0:var
h=d[1];return h===e?[2,f]:e<h?[0,h-1|0]:d;case
1:var
k=d[2],l=d[1];return[1,l,a(iD(j,e,f),k)];case
2:return[2,g(c[i][3],[0,f,0],e-1|0,d[1])];default:var
m=a(c[9],d[1]),n=g(c[i][3],[0,f,0],e-1|0,m);return[3,b(c[65],j,n)]}}function
iD(d,c,b){function
f(a){return fK(d,c,b,a)}return a(e[17][15],f)}function
fL(f,e,d){switch(d[0]){case
0:var
h=d[1];return e<=h?[0,h+f|0]:d;case
1:var
j=d[2],k=d[1];return[1,k,a(fM(f,e),j)];case
2:return[2,g(c[i][2],f,e+1|0,d[1])];default:var
l=a(c[9],d[1]),m=g(c[i][2],f,e+1|0,l);return[3,b(c[65],w[16],m)]}}function
fM(c,b){function
d(a){return fL(c,b,a)}return a(e[17][15],d)}function
cI(b,a){return fL(b,0,a)}function
bH(c,b){return a(fM(c,0),b)}function
iE(m,d,l,k){var
n=k[1],f=l[1],u=k[2],v=l[2],h=m?m[1]:a(A[2],0),p=a(e[17][1],f),i=bK(p,0);function
w(c,b){var
d=b-1|0,e=T(i,d)[d+1];if(e)return c===e[1]?0:a(E[3],rk);var
f=b-1|0;return T(i,f)[f+1]=[0,c]}function
j(a,f,e){if(b(c[44],d,e)){var
g=b(c[65],d,e);if(a<g)if(g<=(p+a|0))return[0,g-a|0,f];return f}function
h(a){return a+1|0}return c0(r[23],d,h,j,a,f,e)}function
y(c,b){var
d=j(0,0,c),f=j(0,0,b);try{var
h=g(e[17][20],w,d,f);return h}catch(b){b=G(b);if(b[1]===bd)return a(E[3],rl);throw b}}var
q=b(c[x],f,h),s=b(c[x],n,h);function
t(d,c,a){var
e=b(be[8][12],be[9],be[8][2]);return o(U[15],e,d,c,a)}function
z(f,e){var
a=dq(rm,q,d,f),g=a[2],b=dq(rn,s,a[1],e),c=b[1],h=b[2],i=t(q,c,g);return y(i,t(s,c,h))}g(e[17][20],z,v,u);function
B(b){return b?[0,b[1]]:a(E[3],ro)}var
C=b(e[19][15],B,i);return aR(0,h,d,f,a(e[19][11],C),n)}function
iF(b){var
a=b;for(;;)switch(a[0]){case
0:return a[1];case
1:return a[1];case
2:return a[1];case
3:return a[1];case
4:var
a=a[2];continue;default:return a[1]}}function
rp(d,c,b){return bk(d,a(e[8],c),b)}function
dt(c){var
d=a(e[17][1],c);return b(r[9],0,d)}function
fN(d){var
c=a(e[17][1],d);function
f(a){return[0,c-a|0]}return b(F[56],c,f)}function
iG(a){function
c(a){return[0,a+1|0]}return b(F[56],a,c)}var
rq=O[2][1];function
rr(c,a){return b(O[2][4],a,c)}var
iH=b(e[17][18],rr,rq);function
em(f,d){var
c=b(e[17][x],f,d),a=c[2],g=c[1];if(a)return[0,g,a[1],a[2]];throw[0,bd,rs]}function
fO(g,f){var
d=0,c=g,b=f;for(;;){if(0===c){if(b){var
h=b[2],i=b[1];return[0,h,i,a(e[17][9],d)]}}else
if(b){var
d=[0,b[1],d],c=c-1|0,b=b[2];continue}throw[0,bd,rt]}}function
fP(d,c){var
f=a(e[17][1],d);function
g(a){return c+(a+1|0)|0}return a(iH,b(F[56],f-c|0,g))}function
iI(d,h){var
e=b(c[3],d,h);if(8===e[0]){var
f=u(ap),i=e[2],j=v===f?ap[1]:l===f?a(t[2],ap):ap;return g(c[a4],d,j,i)}return 0}function
iJ(d,c){var
f=O[2][1],g=1;function
h(f,c,e){return iI(d,a(bG,e))?b(O[2][4],f,c):c}return o(e[17][92],h,g,f,c)}function
iK(j,h,f,d,g){var
k=a(ak,b(e[17][7],f,d-1|0)),m=k[3],n=k[2],o=a(c[i][1],d),l=b(H[16],o,n),p=b(c[i][1],d,m),q=l?bT(j,h,f,l[1],g):O[2][1],r=bT(j,h,f,p,g),s=b(O[2][7],q,r),t=a(O[2][5],d);return b(O[2][7],t,s)}function
bT(f,c,j,e,d){var
h=b(r[36],c,e);if(b(O[2][3],d,h))var
k=g(ea,f,c,e),i=b(r[36],c,k);else
var
i=h;var
l=O[2][1];function
m(b){var
e=iK(f,c,j,b,d);return a(O[2][7],e)}return g(O[2][15],m,i,l)}function
ru(h,a,f){var
d=O[2][1],j=1;function
k(d,a,e){var
j=e[3],k=b(c[i][1],-d|0,f);return g(r[37],h,k,j)?a:b(O[2][4],d,a)}return o(e[17][92],k,j,d,a)}function
iL(h,f,d){var
j=[0,f,1,0];function
k(j,d){var
f=d[2],g=d[1],k=d[3],e=a(ak,j),l=e[3],m=e[2],n=e[1],p=a(c[9],f),q=[0,ay(n,m,o(r[51],h,g,p,l)),k];return[0,b(c[i][1],1,g),f+1|0,q]}return g(e[17][19],k,d,j)[3]}function
fQ(y,x,q,d,h,f,p){var
G=y?y[1]:1,H=x?x[1]:0;bT(q,d,h,p,f);var
I=iJ(d,h),J=bT(q,d,h,p,f),K=b(O[2][7],J,I),L=G?fP(h,f):a(O[2][5],f),s=b(O[2][7],L,K),M=1;function
N(a,c){if(b(O[2][3],a,s))if(a<f){var
e=f-a|0;return b(bb,function(a){var
c=b(r[36],d,a);return b(O[2][3],e,c)?g(ea,q,d,a):a},c)}return c}var
t=g(e[17][77],N,M,h),P=a(e[17][1],t),z=a(O[2][20],s),n=1,u=1,m=0,o=1,l=0,k=0,j=t,Q=P-z|0;for(;;){if(j){var
A=j[2],B=j[1];if(b(O[2][3],n,s)){var
n=n+1|0,R=[0,[0,u],k],u=u+1|0,m=[0,B,m],l=di(a(c[9],(z+Q|0)-(o-1|0)|0),l),k=R,j=A;continue}var
n=n+1|0,m=di(c[14],m),S=[0,[1,o],k],o=o+1|0,l=[0,B,l],k=S,j=A;continue}var
v=a(e[17][9],l),C=a(e[17][9],m),w=a(e[17][1],v),D=a(e[17][9],k),T=1,U=function(b,a){return 0===a[0]?[0,a[1]+w|0,b]:[0,a[1],b]},V=g(e[17][77],U,T,D),W=function(a){return 0===a[0]?[0,a[1]+w|0]:[0,a[1]]},E=b(e[17][15],W,D);if(H)var
X=bk(d,E,p),Y=iL(d,b(c[i][1],-w|0,X),v),F=b(e[18],Y,C);else
var
F=b(e[18],v,C);return[0,[0,F,E,t],V]}}function
fR(m,f,o,l,u,E){var
G=u?u[1]:fP(o,l),H=bT(m,f,o,E,l),p=b(O[2][7],G,H),I=1;function
J(a,c){if(b(O[2][3],a,p))if(a<l){var
d=l-a|0;return b(bb,function(a){var
c=b(r[36],f,a);return b(O[2][3],d,c)?g(ea,m,f,a):a},c)}return c}var
n=g(F[77],J,I,o),v=a(C[1][4],n),w=v-a(O[2][20],p)|0,q=bK(v,rv),d=1,t=0,s=0,h=1,k=0,j=n;for(;;){if(j){var
x=j[2],K=j[1],y=b(bb,a(c[i][1],d),K);if(b(O[2][3],d,p)){var
z=(d+w|0)-h|0;T(q,z)[z+1]=[0,d];var
L=[0,[0,((w+d|0)-h|0)+1|0],k],d=d+1|0,t=[0,y,t],k=L,j=x;continue}var
A=h-1|0;T(q,A)[A+1]=[0,d];var
d=d+1|0,s=[0,y,s],M=[0,[0,h],k],h=h+1|0,k=M,j=x;continue}var
B=a(F[9],k),N=b(e[18],t,s),P=a(F[9],N),Q=a(e[19][11],q),R=1,S=function(d,a){return b(bb,function(e){var
a=bk(f,B,e);return b(c[i][1],-d|0,a)},a)},D=g(F[77],S,R,P),U=aR(0,m,f,D,B,n);return[0,U,aR(0,m,f,n,Q,D)]}}function
iM(b){var
c=fN(b);return a(e[17][9],c)}function
aa(a){return[0,a,iM(a),a]}function
iN(h,k,j,i){try{var
d=[0,h,1],u=function(f,l,e){var
h=e[2],d=e[1];if(h){var
i=a(bG,f),j=a(bG,l),m=g(c[94],k,i,j),n=m||L(U[79],0,d,k,i,j);return[0,b(c[bu],f,d),n]}return[0,d,h]},v=o(e[17][24],u,j,i,d)[2];return v}catch(d){d=G(d);if(d[1]===bd)return 0;var
l=a(g9[1],d),m=b(c[x],i,h),n=a(r[g6],m),p=a(f[49],n),q=b(c[x],j,h),s=a(r[g6],q),t=a(f[49],s);o(g_[3],rw,t,p,l);throw d}}function
iO(d,c,g,e){if(iN(d,c,g[3],e[1]))return 0;var
h=az(d,c,e),i=a(f[3],rx),j=az(d,c,g),k=a(f[3],ry),l=b(f[12],k,j),m=b(f[12],l,i);return b_(rz,b(f[12],m,h))}function
al(g,f,e,c,b){var
i=b[3],j=b[2],k=c[2],l=c[1],h=g?g[1]:0,d=e?e[1]:w[16];if(1-h)iO(f,d,c,b);return aR([0,h],f,d,l,a(el(d,k),j),i)}function
iP(f,c,a){var
d=a[2],g=a[3],h=a[1],i=b(bb,function(a){return bk(f,d,a)},c),j=[0,c,g],k=1;function
l(a){return cI(k,a)}return[0,[0,i,h],[0,rA,b(e[17][15],l,d)],j]}function
iQ(d,a,c,b){function
f(c,b){return iP(a,c,b)}return ds(0,d,a,g(e[17][19],f,b,c))}function
cJ(p,k,f,d,n,h){var
q=p?p[1]:0,j=bc(n),u=a(c[9],d);if(g(c[94],f,j,u))return aa(h);if(o(c[i][14],f,1,d,j)){var
v=d9(d,j,h),w=function(b){var
a=b+1|0;return a===d?cI(-1,n):d<a?[0,a-1|0]:[0,a]},x=a(e[17][1],h);return aR([0,q],k,f,v,b(F[56],x,w),h)}var
l=fQ(0,0,k,f,h,d,j)[1],m=l[2],y=l[3],z=l[1],r=b(e[17][7],m,d-1|0),s=0===r[0]?r[1]:a7(rD),t=bk(f,m,j),A=d9(s,t,z),B=1;function
C(d,a){return fK(f,s,b(c[i][1],-1,t),a)}return aR([0,q],k,f,A,g(e[17][77],C,B,m),y)}var
bl=[e9,rE,e4(0)],aS=[e9,rF,e4(0)];function
en(f,c,i,h,d,b){if(d){if(b){var
k=b[2],l=d[2],g=iR(f,c,i,h,d[1],b[1]),m=g[2],n=g[1],o=function(a){return bk(c,m,a)},j=a(e[17][15],o),p=a(j,l);return al(0,f,[0,c],en(f,c,i,n,p,a(j,k)),g)}}else
if(!b)return aa(h);throw bl}function
iR(i,a,h,f,e,d){if(g(c[94],a,e,d))return aa(f);var
j=b(c[3],a,e);if(0===j[0]){var
k=j[1];if(b(O[2][3],k,h))return cJ(0,i,a,k,[2,d],f);throw aS}var
l=b(c[3],a,d);if(0===l[0]){var
m=l[1];if(b(O[2][3],m,h))return cJ(0,i,a,m,[2,e],f);throw aS}var
n=b(c[82],a,e),o=n[1],r=n[2],p=b(c[82],a,d),q=p[1],s=p[2];if(b(c[56],a,o))if(b(c[56],a,q)){if(g(c[94],a,o,q))return en(i,a,h,f,r,s);throw bl}throw aS}function
iS(c,a){var
d=[0,1,O[2][1]];function
f(c,e,f){var
d=c[2],a=c[1];return 2===e[0]?[0,a+1|0,b(O[2][4],a,d)]:[0,a+1|0,d]}return o(e[17][23],f,d,c,a)[2]}function
iU(a){var
c=O[2][1];function
d(c,a){var
d=iT(a);return b(O[2][7],c,d)}return g(e[17][18],d,c,a)}function
iT(b){switch(b[0]){case
0:return a(O[2][5],b[1]);case
1:return iU(b[2]);default:return O[2][1]}}function
eo(a){return 3===a[0]?1:0}function
ep(c,a){if(c){if(a){var
i=a[2],j=a[1],k=c[2],l=c[1];try{var
t=[0,iV(l,j)],d=t}catch(a){a=G(a);if(a!==aS)throw a;var
d=0}try{var
s=[0,ep(k,i)],f=s}catch(a){a=G(a);if(a!==aS)throw a;var
f=0}if(d)if(f){var
g=f[1],h=d[1],m=g[2],n=g[1],o=h[2],p=h[1],q=b(e[18],h[3],g[3]),r=b(e[18],o,m);return[0,b(e[18],p,n),r,q]}throw aS}}else
if(!a)return rG;throw bl}function
iV(d,a){var
c=d[2],f=d[1];switch(c[0]){case
0:var
g=c[1];if(2!==a[0])return[0,[0,[0,g,a],0],0,0];break;case
1:var
h=c[3],i=c[2],k=c[1];switch(a[0]){case
1:var
l=a[2];if(b(j[46],k,a[1][1]))return ep(h,b(e[17][x],i,l)[2]);throw bl;case
2:break;default:throw aS}break;default:return[0,0,[0,[0,c[1],a],0],0]}return[0,0,0,[0,[0,[0,f,c],a[1]],0]]}function
iW(d,c){var
f=c[2];try{var
g=a(e[17][9],f),h=function(a){return 1-eo(a)},i=[0,ep(d,b(e[17][33],h,g))];return i}catch(a){a=G(a);if(a===bl)return 0;if(a===aS)return 1;throw a}}function
eq(c,a){if(c){if(a){var
i=a[2],j=a[1],k=c[2],l=c[1];try{var
q=[0,iX(l,j)],d=q}catch(a){a=G(a);if(a!==aS)throw a;var
d=0}try{var
p=[0,eq(k,i)],f=p}catch(a){a=G(a);if(a!==aS)throw a;var
f=0}if(d)if(f){var
g=f[1],h=d[1],m=g[1],n=h[1],o=b(e[18],h[2],g[2]);return[0,b(e[18],n,m),o]}throw aS}}else
if(!a)return rI;throw bl}function
iX(c,d){var
a=d[2];switch(c[0]){case
0:return[0,[0,[0,c[1],a],0],0];case
1:var
f=c[2],g=c[1][1];switch(a[0]){case
0:return[0,0,[0,[0,a[1],c],0]];case
1:var
h=a[3],i=a[2];if(b(j[46],a[1],g))return eq(b(e[17][x],i,f)[2],h);throw bl}break;case
2:return rH}throw aS}function
iY(d,c){var
f=d[2];try{var
g=a(e[17][9],f),h=function(a){return 1-eo(a)},i=[0,eq(b(e[17][33],h,g),c)];return i}catch(a){a=G(a);if(a===bl)return 0;if(a===aS)return 1;throw a}}function
fS(k,f,p,h){var
q=b(c[x],f,k),l=[0,0,0,0,0,j[1][10][1]];function
m(a,h){var
f=h[2],d=h[1],k=a[5],e=a[4],l=a[3],m=a[2],n=a[1],g=bc(f);if(0===f[0]){var
r=f[1];return[0,n,m,[0,[0,r,d],l],e,b(j[1][10][4],d,k)]}var
s=o(aq[3],0,q,p,g),t=b(j[1][10][4],d,k),u=b(c[i][1],e,s);return[0,[0,ay([0,d],[0,b(c[i][1],e,g)],u),n],[0,g,m],l,e+1|0,t]}var
d=g(e[17][18],m,l,h),n=d[5],r=d[3],s=d[2],t=d[1],u=[0,n,a(e[17][1],f),0];function
v(m,c){var
h=c[3],d=c[2],f=c[1],g=a(ak,m),i=g[3],k=g[2],n=g[1];try{var
p=[0,f,d-1|0,[0,ay([0,b(e[17][36],d,r)],k,i),h]];return p}catch(a){a=G(a);if(a===X){var
l=b(aV[28],n,f),o=[0,ay([0,l],k,i),h];return[0,b(j[1][10][4],l,f),d-1|0,o]}throw a}}return[0,s,t,g(e[17][19],v,f,u)[3]]}function
er(o,k,j,n,h){var
p=b(c[x],k,j);function
q(f,d){var
e=d[2],g=d[1],h=a(ak,f)[2],j=a(H[7],h);return[0,[0,b(c[i][1],-e|0,j),g],e+1|0]}var
l=g(e[17][19],q,h,rJ),d=l[2],r=l[1];function
s(a){var
b=a[1];return[0,b,cI(d,a[2])]}var
t=b(e[17][15],s,n),f=fS(j,b(e[18],h,k),o,t),m=f[2],u=f[3],v=f[1],w=a(e[17][1],m),y=b(e[18],m,u),z=a(c[i][1],-d|0),A=b(e[17][15],z,v);return[0,y,p,w+d|0,b(e[18],A,r)]}function
iZ(e,a,k,d,j,h){var
f=d[4],l=d[1],p=d[3],q=d[2];if(h){var
r=b(c[i][1],p,h[1]),m=b(B[35],a[1],r),s=function(a){var
d=b(c[x],l,e);return L(a0[18],d,a,[0,k],j,m)},t=b(B[64],s,a)[1];a[1]=aF(ad[29],0,[0,ad[17]],0,0,0,e,a[1]);var
u=g(c[i][3],f,0,t),v=b(B[35],a[1],u),w=g(c[i][3],f,0,m);return[0,v,b(B[35],a[1],w)]}function
y(a){var
d=b(c[x],l,e);return o(a0[17],d,a,[0,k],j)}var
z=b(B[64],y,a)[1],A=g(c[i][3],f,0,z);a[1]=aF(ad[29],0,[0,ad[17]],0,0,0,e,a[1]);var
n=b(B[35],a[1],A);return[0,n,L(ax[2],0,0,q,a[1],n)]}function
fT(b,h,a,g,f,e,d,c){var
i=g[3];return iZ(b,a,i,er(a,h,b,e,d),c,f)}function
i0(j,d,f,N,u,t){try{var
v=a(r[76],t),w=a(r[76],f),k=[0,b(e[18],w,v)],y=function(c){var
d=a(a0[27],c);return d?d:b(e[17][29],c,k[1])},l=function(c){var
a=b(aV[24],c,y);k[1]=[0,a,k[1]];return a},h=b(c[x],f,j),m=ed(h,d[1],u),z=m[2],n=dl(m[1]),p=n[1],A=n[2],B=b(a_[10],h,d[1]),q=b(e[17][15],B,z),C=b(a_[10],h,d[1]),s=b(e[17][15],C,A),D=b(e[18],s,q),E=[0,a(c[26],p),D],F=a(c[34],E),H=bQ(d[1],p),I=b(_[3],h,H),J=function(k,u){var
v=a(c[8],u),w=g(r[58],d[1],v,s),m=b(c[90],d[1],w),y=m[2],z=m[1],A=0;function
B(k,e){var
f=a(ak,k),g=f[3],i=f[2],j=f[1];if(j)return[0,ay([0,l(j[1])],i,g),e];var
m=d[1],n=b(c[x],e,h);return[0,ay([0,l(o(aV[9],n,m,g,0))],i,g),e]}var
f=g(e[17][19],B,z,A),n=b(c[x],f,j),p=ed(n,d[1],y),C=p[2],q=dl(p[1]),t=q[2],i=q[1],D=dt(f),E=b(e[18],t,D),F=[0,a(c[29],[0,i,k+1|0]),E],G=a(c[34],F),H=dr(dt(f)),I=fN(f),J=dr(t),K=b(e[18],J,I);return[0,n,f,G,[1,[0,[0,i[1],k+1|0],i[2]],K],H,C]},K=b(e[19][16],J,I),L=function(g){var
k=g[2],m=g[6],n=g[5],o=g[4],p=g[3];a(e[17][1],f);var
h=a(e[17][1],k),l=b(e[18],k,f);try{var
r=a(c[i][1],h),s=b(e[17][15],r,q),t=bH(h,dr(dt(f))),u=iS(b(e[18],t,n),l),v=[0,[0,en(j,d[1],u,l,s,m),h,p,o]];return v}catch(a){a=G(a);if(a===bl)return 0;if(a===aS)return 1;throw a}},M=[0,[0,F,b(e[19][15],L,K)]];return M}catch(a){a=G(a);if(a===X)return 0;throw a}}function
i1(d,c){var
f=c[2];function
g(h,n){var
a=n;for(;;){if(h){if(a){var
c=a[1],o=h[2],p=h[1];if(3===c[0]){var
a=a[2];continue}var
q=g(o,a[2]),f=p[2];switch(f[0]){case
0:var
d=0;break;case
1:var
i=f[3],k=f[2],l=f[1];switch(c[0]){case
0:var
d=[0,c[1],0];break;case
1:var
m=c[2],d=b(j[46],l,c[1][1])?g(i,b(e[17][x],k,m)[2]):0;break;default:var
d=0}break;default:var
d=0}return b(e[18],d,q)}}else
if(!a)return 0;return 0}}return g(d,a(e[17][9],f))}function
es(e,d){var
f=a(b9,b(c[118],d,e));return a(K[10][8],f)}function
rK(d){function
c(c){return 0===c[0]?b(r[c7],d,c[1]):a(j[1][9],c[1])}function
e(b){return a(f[3],rL)}return b(f[39],e,c)}function
i2(l,k){var
c=l,a=k;for(;;){if(c){if(a){var
d=a[1],g=c[1],m=a[2],n=c[2];if(0===g[0]){var
h=g[1];if(0===d[0])var
e=b(aJ[3],h,d[1]),f=1;else
var
f=0}else{var
i=g[1];if(0===d[0])var
f=0;else
var
e=b(j[1][1],i,d[1]),f=1}if(!f)var
e=0;if(e){var
c=n,a=m;continue}return e}}else
if(!a)return 1;return 0}}function
fU(a){function
d(a){var
f=a[8],g=a[7],h=a[1],d=b(e[17][15],aZ,a[4]),j=b(c[i][11],d,f);return ay([0,h],[0,b(c[i][11],d,g)],j)}return b(e[17][15],d,a)}function
fV(i,d,h,l){var
o=h?h[1]:0;function
m(b){return o?b:a(f[7],0)}function
n(d,c,b){return fI(d,c,a(e[8],b))}function
k(h){switch(h[0]){case
0:var
s=h[4],A=h[2],o=h[1],E=h[3],F=a(e[7],o),p=b(c[x],F,i),G=function(c){var
e=k(c[9]),h=a(f[5],0),i=a(f[3],rM),l=g(r[W],p,d,c[8]),m=a(f[3],rN),n=a(j[1][9],c[1]),o=a(f[3],rO),q=b(f[12],o,n),s=b(f[12],q,m),t=b(f[12],s,l),u=b(f[12],t,i),v=b(f[12],u,h),w=b(f[12],v,e);return b(f[26],2,w)},H=g(f[39],f[5],G,A),I=fU(A),t=b(c[x],I,p),J=az(i,d,o),K=a(f[3],rP),L=m(b(f[12],K,J)),M=a(f[5],0);if(0===s[0])var
N=s[1],O=g(r[W],t,d,E),P=a(f[3],rQ),R=m(b(f[12],P,O)),S=g(r[W],t,d,N),T=a(f[3],rR),U=n(p,d,o),V=b(f[12],U,T),X=b(f[12],V,S),B=b(f[12],X,R);else
var
_=es(t,s[1]),$=a(f[3],rS),aa=n(p,d,o),ab=b(f[12],aa,$),B=b(f[12],ab,_);var
Y=b(f[12],B,M),Z=b(f[12],Y,H);return b(f[12],Z,L);case
1:var
u=h[1],ac=h[4],ad=h[3],ae=h[2],af=a(e[7],u),v=b(c[x],af,i),ag=a(f[7],0),ah=function(e,c){if(c)var
d=k(c[1]);else
var
h=a(f[5],0),i=a(f[3],rT),d=b(f[12],i,h);var
g=b(f[23],2,d);return b(f[12],e,g)},ai=g(e[19][17],ah,ag,ac),aj=a(f[13],0),ak=az(i,d,u),al=a(f[3],rU),am=g(r[W],v,d,ad),an=a(f[3],rV),ao=b(f[12],an,am),ap=b(f[12],ao,al),aq=b(f[12],ap,ak),ar=m(b(f[12],aq,aj)),as=a(f[5],0),at=es(v,ae),au=a(f[3],rW),av=n(v,d,u),aw=b(f[12],av,au),ax=b(f[12],aw,at),ay=b(f[12],ax,as),aA=b(f[12],ay,ar);return b(f[12],aA,ai);case
2:var
C=h[1],aB=h[6],aC=a(e[7],C);b(c[x],aC,i);var
aD=a(f[7],0),aE=function(c,a){var
d=k(a[5]);return b(f[12],c,d)},aF=g(e[17][18],aE,aD,aB),aG=az(i,d,C),aH=a(f[3],rX),aI=a(f[3],rY),aJ=b(f[12],aI,aH),aK=b(f[12],aJ,aG),aL=b(f[12],aK,aF);return b(f[26],2,aL);case
3:var
aM=h[1],aN=k(h[2]),aO=a(f[5],0),aP=az(i,d,aM),aQ=a(f[3],rZ),aR=b(f[12],aQ,aP),aS=b(f[12],aR,aO),aT=b(f[12],aS,aN);return b(f[26],2,aT);case
4:var
aU=h[1],aV=k(h[2]),aW=a(f[5],0),aX=a(j[1][9],aU),aY=a(f[3],r0),aZ=b(f[12],aY,aX),a0=b(f[12],aZ,aW),a1=b(f[12],a0,aV);return b(f[26],2,a1);default:var
l=h[2],w=h[1],y=l[8],D=l[7],z=l[1],a2=h[3],a3=l[10],a4=l[3],a5=l[2],a6=z[3],a7=z[2],a8=z[1],a9=a(e[7],w),q=b(c[x],a9,i),a_=k(a2),a$=a(f[14],0),ba=az(i,d,l[9]),bb=a(f[3],r1),bc=a(f[13],0),bd=az(i,d,D),be=a(f[3],r2),bf=a(f[13],0),bg=a(e[7],y),bh=es(b(c[x],bg,i),a4),bi=a(f[3],r3),bj=a(f[13],0),bk=a(f[13],0),bl=a(e[7],y),bm=b(c[x],bl,i),bn=g(r[W],bm,d,a3),bo=a(f[3],r4),bp=az(i,d,y),bq=a(f[3],r5),br=a(f[13],0),bs=az(i,d,w),bt=a(f[3],r6),bu=a(f[3],r7),bv=g(r[W],q,d,a5),bw=a(f[3],r8),bx=g(r[W],q,d,a6),by=a(f[3],r9),bz=b(f[12],by,bx),bA=b(f[12],bz,bw),bB=b(f[12],bA,bv),bC=b(f[12],bB,bu),bD=b(f[12],bC,bt),bE=b(f[12],bD,bs),bF=b(f[12],bE,br),bG=b(f[12],bF,bq),bH=b(f[12],bG,bp),bI=b(f[12],bH,bo),bJ=b(f[12],bI,bn),bK=b(f[12],bJ,bk),bL=b(f[12],bK,bj),bM=b(f[12],bL,bi),bN=b(f[12],bM,bh),bO=b(f[12],bN,bf),bP=b(f[12],bO,be),bQ=b(f[12],bP,bd),bR=b(f[12],bQ,bc),bS=b(f[12],bR,bb),bT=b(f[12],bS,ba),bU=m(b(f[12],bT,a$)),bV=Q(d,D,a7),bW=g(r[W],q,d,bV),bX=a(f[3],r_),bY=a(j[1][9],a8),bZ=a(f[3],r$),b0=n(q,d,w),b1=b(f[12],b0,bZ),b2=b(f[12],b1,bY),b3=b(f[12],b2,bX),b4=b(f[12],b3,bW),b5=b(f[12],b4,bU),b6=b(f[12],b5,a_);return b(f[26],2,b6)}}return k(l)}function
sb(d){var
c=a(A[2],0),e=fV(c,a(w[17],c),0,d);return b(f[48],sa[9][1],e)}function
sc(f,d,k,a){function
g(d,a){var
h=b(c[3],f,a);if(0===h[0]){var
j=h[1]-d|0;if(0<=j)try{var
l=b(e[17][36],j,k),m=b(c[i][1],d,l);return m}catch(b){b=G(b);if(b===X)return a;throw b}return a}function
n(a){return a+1|0}return L(c[fd],f,n,g,d,a)}return g(d,a)}function
sd(a){var
c=a[2];function
d(a){switch(a[0]){case
0:return 1;case
1:return 0;default:return 1}}return b(e[17][25],d,c)}function
fW(c){var
d=[0,j[1][10][1],0];function
f(h,d){var
e=d[1],i=d[2],c=a(ak,h),f=c[1],k=c[3],l=c[2];if(f){var
g=b(aV[25],f[1],e),m=[0,ay([0,g],l,k),i];return[0,b(j[1][10][4],g,e),m]}throw[0,P,se]}return g(e[17][19],f,c,d)[2]}function
et(k,j,q){var
c=k[2],d=k[1],f=fO(j-1|0,q),g=f[3],l=f[2],h=f[1],i=a(ak,l),m=i[1],r=i[2],n=i0(d,c,h,m,i[3],g);function
s(a){if(typeof
a==="number"){if(0===a)return 0;throw[0,P,sf]}var
f=a[1],i=f[1],j=i[2],k=f[2],m=i[1],n=ek(c[1],j,f[4]),o=fW(m),p=[0,o,[0,n,b(e[17][x],k,j)[2]],[0,l,h]],q=ds(0,d,c[1],p);return[0,iQ(d,c[1],q,g)]}if(n){var
o=n[1],p=o[2],t=o[1],u=function(a){return 1===a?1:0};if(b(e[19][32],u,p))return 0;var
v=[0,ay(m,r,t),h],w=b(e[18],g,v),y=b(e[19][15],s,p);return[0,[0,j,fW(w),y]]}return 0}function
i3(f,c){function
g(a){return a+1|0}var
h=a(e[17][1],c),i=b(F[56],h,g);function
j(d){var
a=et(f,d,c);if(a){var
e=a[1][3],g=function(a){return 0===a?1:0};return b(bm[34],g,e)}return 0}var
d=b(e[17][33],j,i);return d?[0,d[1]]:0}function
i4(c){function
f(d,c){function
h(d,c){switch(d[0]){case
0:return[0,[0,d[1],0],c];case
1:var
g=f(0,a(e[17][9],d[2]));return b(e[18],g,c);case
2:return c;default:return[0,[0,d[1],1],c]}}return g(e[17][19],h,c,d)}var
d=f(0,c);function
h(b,a){return b[1]-a[1]|0}return b(e[17][46],h,d)}function
sg(a){var
b=a[1];return a[2]?[3,b]:[0,b]}var
sh=a(e[17][15],sg);function
si(e,d){return b(bb,a(c[i][1],e),d)}function
eu(e){var
h=1;return function(i){var
c=h,a=i;for(;;){if(a){var
d=a[1],f=a[2],g=aZ(d);if(b(j[1][1],e,g))return[0,c,d];var
c=c+1|0,a=f;continue}throw X}}}function
i5(w,l,k,d){var
h=cA(0,function(a){throw[0,bd,sj]},k),i=h[3],m=h[1],n=a(c[67],l),j=b(e[17][15],n,m),o=0;function
p(d,c){return[0,a(eu(b(e[17][7],j,d[1]-1|0)),i)[2],c]}var
f=g(e[17][19],p,d,o),q=1;function
r(g,e){var
c=a(eu(e),f)[1];function
h(a){var
b=a[2];if(a[1]===g){var
d=b?[3,c]:[0,c];return[0,d]}return 0}return b(F[c3],h,d)}var
s=g(e[17][77],r,q,j),t=1;function
u(c,e){var
f=a(eu(a(bF,e)[1]),i)[1];function
g(a){var
b=a[2];if(a[1]===f){var
d=b?[3,c]:[0,c];return[0,d]}return 0}return b(F[c3],g,d)}var
v=g(e[17][77],u,t,f);return[0,cB(f)[1],s,v]}function
i6(f,d,e){if(0===a(c[mx],d))return b(c[x],f,d);var
g=hx(e),h=fz(fq,[0,hw(e)],g),i=b(c[c2],h,d);return b(c[x],f,i)}function
fX(f,d){function
g(e){var
g=a(fx,e),d=u(bE),h=b(c[5],f,g),i=v===d?bE[1]:l===d?a(t[2],bE):bE;return b(au[11],i,h)}return b(e[17][aY],g,d)}function
fY(m,i,h,g){var
d=g[1],n=g[2],o=m[1],p=b(c[x],d,i),k=cG(i,h,d);if(a(e[17][55],d))var
l=k;else
var
v=a(f[5],0),w=a(f[3],sl),y=a(f[5],0),z=b(f[12],y,w),A=b(f[12],z,v),l=b(f[12],A,k);var
q=fI(p,h,n),r=a(f[3],sk),s=a(j[1][9],o),t=b(f[12],s,r),u=b(f[12],t,q);return b(f[12],u,l)}function
sm(d,c){var
f=b(e[17][7],d,c-1|0),g=a(e[7],f);return a(K[10][16],g)}var
i7=a(e[17][19],c[c2]);function
fZ(h,g){function
i(a){return 1-a[2]}var
c=b(e[17][33],i,g);if(c){var
d=c[1][1],j=d[1],k=dp(h,d),l=a(f[3],sn);return bR([0,[0,j],so,b(f[12],l,k)])}return 0}function
cK(h,d,q,bi,bh,J,y,w,p){var
L=bi,A=bh;for(;;){var
ap=y[3],aq=y[2],bj=y[1];if(A){var
ar=A[2],as=A[1],_=as[1],m=_[3],N=_[2],$=_[1],ab=iW(N,y);if(typeof
ab==="number"){if(0===ab){var
L=[0,as,L],A=ar;continue}var
bl=i1(N,y),bm=0,bn=function(f,l){if(f)return f;var
i=et([0,h,d],l,a(e[7],y));if(i){var
c=i[1],j=[0,c[2],aq,ap],m=c[3],n=c[1];try{var
o=function(g,f){if(f){var
b=f[1],k=a(e[8],b),l=bk(d[1],k,p),m=a(e[8],b),n=cH(d[1],m,w),c=cK(h,d,q,0,g,J,al(0,h,[0,d[1]],b,j),n,l);if(c){var
i=c[1];return[0,i[1],[0,i[2]]]}throw X}return[0,g,0]},r=a(e[17][9],L),s=b(e[18],r,A),k=g(e[19][66],o,s,m),t=[0,[0,k[1],[1,j,n,p,k[2]]]];return t}catch(a){a=G(a);if(a===X)return 0;throw a}}return 0};return g(e[17][18],bn,bm,bl)}var
ac=ab[1],s=[0,bj,aq,ap],D=ac[1],au=s[3],n=s[1],bw=ac[3],bx=ac[2],by=s[2],F=i6(n,h,d),bB=function(j){var
c=j[2],e=j[1],i=fT(h,n,d,q,0,D,w,e)[1];if(2===c[0]){var
k=c[1],l=aF(U[83],0,0,0,F,d[1],i,k),B=l[1];if(l[2]){d[1]=B;return 0}var
C=g(r[W],F,d[1],k),E=a(f[3],sw),G=a(f[13],0),H=g(r[W],F,d[1],i),I=a(f[3],sx),J=b(f[12],I,H),K=b(f[12],J,G),L=b(f[12],K,E),M=b(f[12],L,C),N=a(cF[6],e);return g(af[6],N,sy,M)}var
m=bc(c),o=g(r[W],F,d[1],m),p=a(f[3],st),s=a(f[13],0),t=g(r[W],F,d[1],i),u=a(f[3],su),v=b(f[12],u,t),x=b(f[12],v,s),y=b(f[12],x,p),z=b(f[12],y,o),A=a(cF[6],e);return g(af[6],A,sv,z)},bC=function(e){var
j=e[1],k=j[2],l=j[1],o=e[2];if(a(H[3],l))return 0;if(0===k[0])if(0!==k[2])return 0;var
m=er(d,n,h,D,w),p=m[1],q=g(c[i][3],m[4],0,o),s=d[1],t=b(c[x],p,h),u=g(r[W],t,s,q),v=a(f[3],sz),y=b(f[12],v,u);return g(af[6],l,sA,y)};b(e[17][14],bB,bx);b(e[17][14],bC,bw);switch(m[0]){case
0:var
ax=m[2],bE=m[1],Z=er(d,n,h,D,w),an=Z[4],ba=Z[2],ao=Z[1],cM=Z[3],bb=cA(0,function(a){throw[0,bd,sO]},ao),be=bb[3],bf=bb[1],cN=b(i7,be,h),cO=function(h,r){var
m=r[1],s=m[1],f=s[2],t=h[5],j=h[4],n=h[2],C=r[2],D=m[4],E=m[3],F=s[1],G=h[3],H=h[1],I=o(a0[25],0,0,0,j),u=g(B[65],I,d,E)[2],v=u[1],K=u[2],L=v[2],M=a(a0[16],v[1]);function
N(a){return b(M,a,0)}var
O=g(B[65],N,d,D),w=d8(n,an,L),P=a(e[17][1],w)+n|0,Q=g(c[i][3],an,P,O),x=b(B[41],d[1],w),p=b(B[35],d[1],Q),k=b(c[37],p,x),R=[0,f,0,aF(a0[3],j,d[1],0,0,[0,f,0],[0,k,0],[0,K,0])],y=aa(x),S=a(c[67],d[1]),T=b(e[17][15],S,bf),z=b(c[i][11],T,k),A=cC(t,d[1],[0,[0,[0,F],[3,sP,[0,f]]]],z),l=A[2];d[1]=A[1];var
q=[0,[0,b(c[75],d[1],l)[1]],J],U=ev(j,d,R,C,q,y,p),V=ay([0,f],[0,l],z),W=fz(f,[0,b(c[i][4],bf,l)],k);return[0,[0,V,H],n+1|0,[0,[0,f,q,q,be,y,p,l,k,U],G],b(c[c2],W,j),t]},bg=g(e[17][18],cO,[0,ao,0,0,cN,ba],ax),cP=bg[3],cQ=bg[1];b(c[x],ao,h);var
bF=a(e[17][1],ax),bG=[0,b(c[i][1],bF,p)],aB=iZ(h,d,a(e[9],q),[0,cQ,ba,cM,an],bE,bG),z=[0,[0,s,cP,aB[2],[0,aB[1]]]];break;case
1:var
aC=m[1],aD=aC[2],bI=aC[1],aw=b(e[17][36],aD,D);if(0===aw[0])var
aE=aw[1];else
var
bz=a(j[1][9],aD),bA=a(f[3],sr),aE=bR([0,[0,bI],ss,b(f[12],bA,bz)]);var
z=[0,[0,s,0,p,[1,aE]]];break;case
2:var
aG=m[3],aH=m[2],aI=m[1],bJ=m[4],ad=aG?aG[1][2]:a(e[7],q);if(aH){var
aJ=aH[1];o(a0[14],h,d[1],0,aJ);var
aK=hG(aI,ad,aJ)}else
var
aK=hF(aI,ad);var
aL=cK(h,d,q,0,[0,[0,[0,$,N,[4,[0,aK],bJ]],0],0],[0,[1,ad],J],s,w,p);if(aL)var
bL=aL[1][2],z=[0,[4,a(e[7],q),bL]];else
var
z=0;break;case
3:var
bM=m[2],aM=fT(h,n,d,q,0,D,w,m[1]),aN=aM[2],ae=aM[1],ag=i4(by),aO=i5(h,d[1],n,ag),aP=aO[2],R=aO[1],O=ds(0,h,d[1],[0,R,aP,n]),bN=a(r[76],R),bO=a(j[1][10][35],bN),bP=a(j[1][6],sB),aQ=b(aV[25],bP,bO),aS=[0,[0,[0,aQ],Q(d[1],O,aN)],R],bQ=Q(d[1],O,ae),ah=b(c[i][1],1,bQ),aT=fQ(sD,sC,h,d[1],aS,1,ah),S=aT[2],I=aT[1],bS=function(a){return 1===a[2]?1:0},bT=b(e[17][31],bS,S)[1],bU=a(e[7],I),bV=h6(bT,Q(d[1],I,ah),bU),bW=bH(1,aP),bX=a(e[8],I),bY=[0,bV,a(el(d[1],bX),bW),R],aU=al(0,h,[0,d[1]],bY,O),ai=a(e[7],I),bZ=dt(ai),b0=function(g){var
f=b(c[65],d[1],g),h=a(e[8],aU);function
i(a){return 3===a[0]?f===a[1]?1:0:0}return b(e[17][26],i,h)?[3,f]:[0,f]},aW=[0,ai,b(e[17][17],b0,bZ),ai],b1=b(c[x],aS,h),b2=Q(d[1],O,p),b3=b(c[i][1],1,b2),b4=g(a_[9],b1,d[1],b3),b5=g(r[50],d[1],ah,b4),aX=Q(d[1],I,b5),b6=function(b,a){return a[1]-b[1]|0},b7=b(e[17][46],b6,S),b8=function(a){return a[2]},b9=b(e[17][17],b8,b7),V=function(i,k){var
c=[0,-1],l=a(j[1][6],sE);function
q(e){c[1]++;var
d=a(E[22],c[1]);return b(K[5],l,d)}function
m(k){var
c=k[3],l=k[2],n=k[1],r=a(e[17][1],l)-i|0,o=b(e[17][x],r,l),m=o[2],t=o[1];if(m){var
u=m[2],v=m[1],p=iY(s,t);if(typeof
p==="number"){var
w=cE(h,l),y=a(f[13],0),z=a(f[3],sF),A=a(f[5],0),B=az(h,d[1],s),C=a(f[13],0),D=a(f[3],sG),E=a(f[5],0),F=a(f[3],sH),H=b(f[12],F,E),I=b(f[12],H,D),J=b(f[12],I,C),K=b(f[12],J,B),L=b(f[12],K,A),M=b(f[12],L,z),N=b(f[12],M,y),O=b(f[12],N,w);return g(af[6],[0,n],sI,O)}var
Q=p[1][1],R=function(a){if(1===a)return[0,v];function
c(b){var
c=b[1]===(a-1|0)?1:0,d=b[2],e=c?d:c;return e}if(b(e[17][26],c,ag))return 0;try{var
d=[0,[0,av,b(e[17][36],a-1|0,Q)]];return d}catch(a){a=G(a);if(a===X)return[0,[0,av,[0,q(0),1]]];throw a}},S=b(e[17][72],R,b9);switch(c[0]){case
2:var
U=c[3],W=c[2],Y=c[1],j=[2,Y,W,U,V(i,c[4])];break;case
3:var
Z=c[1],j=[3,Z,V(i+1|0,c[2])];break;case
4:var
_=c[1],j=[4,_,V(i,c[2])];break;default:var
j=c}var
T=a(e[17][9],S);return[0,[0,n,b(e[18],T,u),j]]}throw[0,P,sJ]}return b(e[17][72],m,k)},b$=V(1,bM),ca=function(b,a){return a[1]-b[1]|0},aY=[0,0],cb=b(e[17][46],ca,S),cc=function(d){var
f=d[2],g=d[1];if(1===f){aY[1]=a(e[17][1],S)-g|0;return ae}var
h=b(e[17][7],ag,(f-1|0)-1|0)[1];return a(c[9],h)},cd=b(e[17][15],cc,cb),cf=aY[1],cg=a(aA[9],h),ch=function(b){var
d=a(C[2][1][1],b);return a(c[10],d)},ci=b(e[17][15],ch,cg),cj=a(e[19][12],ci),aj=fh(0),a1=[0,[0,aj],J],ck=a(e[17][1],w),cl=fS(h,n,d,D)[2],cm=a6(1,w),cn=a6(ck+1|0,cl),co=b(e[18],cn,cm),cp=a(e[8],I),cq=cH(d[1],cp,co),cr=function(a){return[0,a,0]},a2=cK(h,d,q,0,b(e[17][15],cr,b$),a1,aW,cq,aX);if(a2){var
a3=a2[1],cs=a3[2];fZ(h,a3[1]);var
z=[0,[5,s,[0,[0,aQ,ae,aN],p,cf,a1,aj,[0,a(c[12],[0,aj,cj]),cd],O,aW,aU,aX],cs]]}else
var
z=0;break;default:var
ak=m[1],ct=m[2],a4=g(B[62],F,d[1],p),cu=a4[2],am=a(c[c5],a4[1]),cv=fX(d[1],am)[1],cw=b(e[17][15],aZ,am),cx=a(j[1][10][35],cw),a5=0===ak[0]?o(M[13][25],j[1][11][1],cx,0,ak[1]):a(M[13][22],ak[1]),cy=a(c[116],am),a7=b(aA[42],cy,h),a8=b(k[3],d[1],[0,[0,a7,cu],0]),a9=a8[1],Y=g(k[15],a7,a5,a8[2])[2],a$=a(k[1],Y),cz=a$[1];d[1]=a$[2];if(a(k[5],Y))var
cD=b(k[7],a9,Y),z=[0,[0,s,0,p,[0,a(e[17][5],cD)]]];else
var
cG=function(o){var
O=b(i8[3][2],d[1],o),Q=a(c[c5],O),R=b(i8[3][4],d[1],o),z=fX(d[1],Q)[1],A=cB(z),g=A[1],p=b(c[i][11],A[2],R),r=b(c[3],d[1],p);if(9===r[0]){var
s=r[2],B=u(bD),S=r[1],U=v===B?bD[1]:l===B?a(t[2],bD):bD;if(fB(d[1],U,S)){var
x=T(s,1)[2],V=T(s,2)[3],y=ce(d[1],V);if(a(e[8],q)){var
W=b(c[82],d[1],x)[2],X=d[1],Y=function(a){return ce(X,a)},C=b(e[17][17],Y,W),Z=-1,_=function(a){return cI(Z,a)},ab=b(e[17][15],_,C),D=a(e[17][6],g),ac=d[1],k=bK(a(e[17][1],D),rB),M=function(b,a){switch(a[0]){case
0:var
c=a[1]-1|0;return T(k,c)[c+1]=[0,b+1|0];case
3:var
d=a[1]-1|0;return T(k,d)[d+1]=[3,b+1|0];default:throw[0,P,rC]}};b(e[17][89],M,ab);var
E=aR(0,h,ac,n,a(e[19][11],k),D)[2],ad=[0,a(e[17][5],g),0],ae=cH(d[1],E,ad),F=b(e[18],ae,n),af=aR(0,h,d[1],g,[0,y,C],F),ag=3===y[0]?x:p,ah=iM(n),ai=1,aj=function(a){return cI(ai,a)},ak=b(e[17][15],aj,ah),al=aR(0,h,d[1],F,ak,n),am=a(e[17][6],g),G=[0,ag,af,al,[0,aR(0,h,d[1],n,E,am)]]}else
var
aG=iG(a(e[17][1],g)),L=[0,y,a(e[17][6],aG)],aH=a(e[17][6],g),G=[0,x,[0,g,L,g],[0,g,a(e[17][6],L),aH],0];var
j=G}else
var
aI=a(e[19][11],s),aJ=d[1],aK=function(a){return ce(aJ,a)},aL=[0,g,b(e[17][17],aK,aI),au],j=[0,p,aL,aa(au),0];var
H=j[2],an=j[4],ao=j[3],ap=j[1],aq=function(a){return[0,a,0]},I=cK(h,d,q,0,b(e[17][15],aq,ct),J,H,w,ap);if(I){var
K=I[1],ar=K[2],as=K[1],at=function(b){if(0===a(fy,b)){var
d=aZ(b);return[0,a(c[10],d)]}return 0},av=b(e[17][72],at,z),aw=a(e[17][9],av);fZ(h,as);return[0,o,aw,ao,an,ar]}var
ax=az(h,d[1],H),ay=a(f[3],sL),aA=dp(h,[0,$,N,m]),aB=a(f[5],0),aC=a(f[3],sM),aD=b(f[12],aC,aB),aE=b(f[12],aD,aA),aF=b(f[12],aE,ay);return fA(sN,b(f[12],aF,ax))}throw[0,bd,sK]},cJ=b(e[17][15],cG,cz),cL=a(k[70][8],a5),z=[0,[2,s,p,b(e[17][15],aZ,cv),cL,[0,a9,Y],cJ]]}if(z){var
bo=z[1],bp=a(e[17][9],L);return[0,[0,b(e[18],bp,[0,[0,[0,$,N,m],1],ar]),bo]]}return 0}var
at=i3([0,h,d],a(e[7],y));if(at){var
bq=[0,y,0,p,[1,at[1]]],br=a(e[17][9],L);return[0,[0,b(e[18],br,A),bq]]}var
bs=fY(q,h,d[1],y),bt=a(f[5],0),bu=a(f[3],sp),bv=b(f[12],bu,bt);return b_(sq,b(f[12],bv,bs))}}function
ev(c,h,g,m,l,d,k){function
n(a){return[0,a,0]}var
i=cK(c,h,g,0,b(e[17][15],n,m),l,d,0,k);if(i){var
j=i[1],o=j[2];fZ(c,j[1]);return o}var
p=fY(g,c,h[1],d),q=a(f[5],0),r=a(f[3],sQ),s=b(f[12],r,q);return b_(sR,b(f[12],s,p))}aH(1067,[0,ip,iq,bc,dq,it,iu,iv,dr,iw,ce,iy,iz,iA,cG,q2,az,q8,iB,ds,aR,a9,ej,rg,ek,bk,el,cH,Q,fK,iD,fL,fM,cI,bH,iE,rK,i2,fV,sb,iF,rp,dt,fN,iG,iH,em,fO,fP,iI,iJ,iK,bT,ru,iL,fQ,fR,aa,iN,iO,al,iP,iQ,cJ,bl,aS,iR,en,iS,iT,iU,eo,iV,ep,iW,iX,eq,iY,fS,fT,i0,i1,es,sc,sd,fW,et,i3,i4,sh,si,eu,i5,i6,fX,fY,sm,i7,fU,er,cK,ev],mv);function
i9(b,e){var
d=u(b),f=v===d?b[1]:l===d?a(t[2],b):b,g=[0,a(au[10],f),e];return a(c[28],g)}function
i_(b,e){var
d=u(b),f=v===d?b[1]:l===d?a(t[2],b):b,g=[0,a(au[8],f),e];return a(c[23],g)}function
cL(e,d,b){var
f=[0,ar(e,d),b];return a(c[21],f)}function
sS(d,c,b){return cL(d,c,a(e[19][12],b))}function
sT(f,b){var
d=b[2],e=u(ae),g=[0,d,a(c[19],[0,b[1],d,b[3]])],h=v===e?ae[1]:l===e?a(t[2],ae):ae;return cL(f,h,g)}function
ew(f,d,e,h){function
j(m,h,k,r){var
i=b(c[3],d[1],k);if(9===i[0]){var
e=i[2],n=u(aw),s=i[1],w=v===n?aw[1]:l===n?a(t[2],aw):aw;if(g(c[a4],d[1],w,s))if(4===e.length-1){var
x=T(e,1)[2],y=L(ax[2],0,0,m,d[1],x),f=b(c[3],d[1],y);if(6===f[0]){var
o=f[1],p=u(aD),z=f[3],A=f[2],B=v===p?aD[1]:l===p?a(t[2],aD):aD,q=u(ai),C=a(c[24],[0,B,h]),D=v===q?ai[1]:l===q?a(t[2],ai):ai,E=a(c[24],[0,D,h]),F=T(e,3)[4],G=$([0,o,0,A]),H=j(b(c[bu],G,m),E,F,z),I=T(e,0)[1];return[0,[0,o,T(e,2)[3],C,I],H]}throw[0,bd,sU]}}return[0,[0,0,k,h,r],0]}return j(f,h,e,L(ax[2],0,0,f,d[1],e))}function
ex(d,j){var
h=u(ae),k=v===h?ae[1]:l===h?a(t[2],ae):ae,e=b(c[3],d,j);if(9===e[0]){var
f=e[2],i=e[1];if(g(c[a4],d,k,i))if(2===f.length-1){var
m=b(c[76],d,i)[2],n=T(f,1)[2];return[0,[0,m,T(f,0)[1],n]]}}return 0}function
sV(j,d,g){var
f=b(c[3],j,d);switch(f[0]){case
11:var
h=f[1][1];break;case
12:var
h=f[1][1][1];break;default:return[0,d,g]}var
k=a(A[27],h)[1][7],i=b(e[19][54],k,g),l=i[2];return[0,a(c[21],[0,d,i[1]]),l]}function
cM(d,h){if(h){var
f=h[2],j=h[1];if(f){var
k=a(ak,j),m=k[3],w=k[1],x=a(e[17][1],f)+1|0,y=[0,m,0],z=function(e,k){var
m=e[2],n=e[1],f=a(ak,k),g=f[3],h=a(c[19],[0,f[1],g,n]),i=u(ae),o=[0,g,h],p=v===i?ae[1]:l===i?a(t[2],ae):ae,j=cL(d,p,o),q=b(c[73],d[1],j)[1];return[0,j,[0,[0,b(c[76],d[1],q)[2],h],m]]},n=g(e[17][18],z,y,f),p=n[2],A=n[1],B=[0,a(c[9],1),2],C=function(g,f){var
e=f[2],k=f[1],m=g[1],h=b(c[i][1],e,g[2]),n=b(c[71],d[1],h)[2],j=u(aw),o=[0,n,h,a(c[9],e),k],p=v===j?aw[1]:l===j?a(t[2],aw):aw,q=[0,dk(d[1],[0,p,m]),o];return[0,a(c[21],q),e+1|0]},D=g(e[17][19],C,p,B)[1],E=[0,a(c[9],1),1,0],F=a(e[17][9],p),G=function(y,m,d){var
e=d[2],f=d[1],n=d[3],h=a(ak,m),j=u(aD),o=h[3],p=h[1],q=v===j?aD[1]:l===j?a(t[2],aD):aD,k=u(ai),r=a(c[24],[0,q,f]),s=v===k?ai[1]:l===k?a(t[2],ai):ai,w=a(c[24],[0,s,f]),x=[0,$([0,p,[0,r],g(c[i][2],1,e,o)]),n];return[0,b(c[i][1],1,w),e+1|0,x]},q=o(e[17][24],G,F,f,E),H=q[3],I=q[1];return[0,A,[0,$([0,w,[0,I],g(c[i][2],1,x,m)]),H],D]}var
r=a(ak,j),s=r[3],J=r[1],K=a(c[9],1),L=b(c[i][1],1,s);return[0,s,[0,$([0,J,[0,a(c[9],1)],L]),0],K]}throw[0,P,sW]}function
f0(aj,r,d,n,m){var
s=b(c[x],n,r),E=L(ax[2],0,0,s,d[1],m),F=g(U[68],s,d[1],E)[1],p=cM(d,ec(d[1],F)),h=p[2],k=p[1],G=p[3],f=a(e[17][1],h),I=a8(0,f),J=[0,b(c[i][1],f+1|0,m),I],K=a(c[21],J),M=b(c[37],K,h),N=[0,[0,a(j[1][6],sX)],k,M],w=a(c[19],N),y=u(ae),O=[0,k,w],P=v===y?ae[1]:l===y?a(t[2],ae):ae,z=u(aD),Q=cL(d,P,O),R=v===z?aD[1]:l===z?a(t[2],aD):aD,A=u(ai),S=v===A?ai[1]:l===A?a(t[2],ai):ai;function
T(b){var
c=a(ak,b),d=a(e[8],c);return a(H[7],d)}var
V=g$(b(e[17][15],T,h));function
W(d){var
f=a(e[17][5],d),g=a(e[17][6],d);return b(c[i][4],g,f)}var
X=b(e[17][17],W,V),q=b(c[i][1],f+1|0,k),Y=d2(1,f+1|0,h),Z=a8(0,f),_=[0,b(c[i][1],2*(f+1|0)|0,m),Z],$=a(c[21],_),aa=b(c[37],$,Y),ab=[0,[0,a(j[1][6],sY)],q,aa],ac=a(c[19],ab);b(c[i][1],f,q);var
ad=a(c[9],1),C=u(aw),af=[0,q,ac,b(c[i][1],1,G),ad],ag=v===C?aw[1]:l===C?a(t[2],aw):aw,ah=cL(d,ag,af),D=b(c[38],w,n);o(aq[3],0,r,d,D);a(B[48],d);return[0,k,D,n,X,R,S,ah,Q]}function
i$(b){return a(ao[41],[2,b])}var
bU=[l,function(a){return ac(s0,sZ)}],s3=[l,function(a){return ac(s2,s1)}],s6=[l,function(a){return ac(s5,s4)}];function
du(m,i,d){var
n=a(A[27],d[1])[1],o=bQ(i,d),p=a(_[41],o),g=ec(i,b(e[17][15],c[W],p)),q=n[7],j=a(e[17][1],g)-q|0;if(0===j)bR([0,0,s8,a(f[3],s7)]);var
k=b(e[17][x],j,g)[2],r=V(0,k),s=[0,a(c[26],d),r],t=a(c[21],s),u=V(0,g),v=[0,a(c[26],d),u],l=[0,i],w=a(c[21],v),h=f0(0,m,l,k,t);return[0,l[1],h[2],h[3],w,h[7],g,j,h[1]]}function
ja(e,d){var
f=b(c[5],e,d);return a(c[8],f)}function
f1(o,I,f,n){var
p=n[1],d=du(o,I,[0,p,n[2]]),q=d[7],h=d[6],r=d[1],J=d[8],L=d[5],M=d[4],N=d[3],O=d[2],g=i$(p),P=b(a_[9],o,r),e=a(w[ld],r),s=ja(e,M),Q=ja(e,J),R=bw(b(K[5],g,s9),O,0,f,e,s_),x=b(K[5],g,s$),T=[0,$([0,[0,b(K[5],g,ta)],0,s]),h],U=bw(x,a(P,b(c[38],L,T)),0,f,e,tb);if(f)var
y=e;else
var
ai=a(A[2],0),y=a(w[17],ai);var
j=u(bU),E=v===j?bU[1]:l===j?a(t[2],bU):bU,k=a5(y,E),m=k[1],F=b(ad[11],m,k[2]),G=a(H[7],F)[2][1],z=a(A[2],0),B=c0(w[mb],0,0,0,z,m,[1,R]),W=B[2],C=c0(w[mb],0,0,0,z,B[1],[1,U]),X=C[2],Y=C[1],D=b(K[5],g,tc),Z=V(0,h),_=[0,a(c[8],X),Z],aa=[0,a(c[21],_),0],ab=V(q,N),ac=[0,a(c[8],W),ab],ae=[0,a(c[21],ac),aa],af=da(D,f,Y,h,G,[0,s,[0,b(c[i][1],q,Q),ae]]),ag=[0,b(S[1],0,[1,x]),0];b(cN[2][88],1,ag);var
ah=[0,b(S[1],0,[1,D]),0];b(cN[2][88],1,ah);return af}function
td(d,c,b,a){f1(d,c,b,a);return 0}cb([0,te,function(a,b){return ca(td,a,b)}]);function
jb(g,d,h){try{var
r=ib(g,d,[0,[0,av,0]],w[aY]),i=r[2][1],_=r[1];b(c[75],d,i);var
s=u(bU),aa=v===s?bU[1]:l===s?a(t[2],bU):bU,x=a5(_,aa),ab=x[1],y=a(c[21],[0,x[2],[0,h,i]]),z=o(ad[30],0,g,ab,y),A=z[2],n=z[1],ac=b(c[73],d,y)[1],C=b(c[76],d,ac)[2],ae=[0,i_(s3,C),[0,h,i,A]],af=a(c[21],ae),ag=[0,i_(s6,C),[0,h,i,A]],ah=a(c[21],ag),ai=b(B[35],n,ah),aj=[0,n,b(B[35],n,af),ai];return aj}catch(i){i=G(i);if(i===X){var
D=b(c[5],d,h),p=b(cO[1],g,D),m=p[1],E=p[2],j=du(g,d,c$(m)),k=j[1],F=j[6],H=j[5],I=j[2],J=a(f[3],tf),K=b(bj[66],g,m),L=a(f[3],tg),M=b(bj[66],g,m),N=a(f[3],th),O=b(f[12],N,M),P=b(f[12],O,L),Q=b(f[12],P,K),R=b(f[12],Q,J);b(at[8],0,R);var
S=[0,$([0,0,0,h]),F],T=b(c[38],H,S),q=b(e[17][15],c[8],E),V=b(U[55],k,[0,T,q]),W=b(B[35],k,V),Y=[0,I,a(e[19][12],q)],Z=a(c[21],Y);return[0,k,b(B[35],k,Z),W]}throw i}}function
jc(j,u,t,i,r){var
f=[0,r],h=ew(i,f,u,a(c[10],t));if(j)var
d=h;else{if(h)if(h[2])var
q=h[1],F=ew(i,f,q[2],q[3]),d=b(e[18],F,h),n=1;else
var
n=0;else
var
n=0;if(!n)var
d=h}if(j)var
l=d;else{if(d)if(d[2])var
p=d[1],E=ew(i,f,p[2],p[3]),l=b(e[18],d,E),o=1;else
var
o=0;else
var
o=0;if(!o)var
l=d}function
v(d){var
h=d[2],j=a(s[47],d[3]),e=b(c[5],f[1],h);return[0,g(ti[8],i,f[1],e),j]}var
w=b(e[17][15],v,l);function
x(a){return z(g(s[71],[0,a[1]],a[2],dv[6]))}var
y=j?e[17][17]:e[17][15],A=b(y,x,w),B=a(m[7],A),C=a(cz[11],f[1]),D=b(m[5],C,B);return b(k[70][1],0,D)}function
tj(s,d,p,j){function
k(o,B,n,m,l,f,k){var
e=b(c[71],d,f),p=e[3],q=[0,$([0,e[1],0,e[2]]),0],h=[0,$([0,n,0,p]),q],r=a(c[9],1),s=a(c[9],2),t=b(c[i][1],2,f),u=[0,b(c[i][1],2,l),t,s,r],v=[0,i9(aw,m),u],j=a(c[21],v),w=[0,b(c[i][1],2,o),[0,j]],x=a(c[21],w),y=b(c[38],x,h),z=g(c[i][2],2,2,k),A=b(c[i][5],j,z);return[0,y,b(c[37],A,h)]}function
l(i,f){var
g=b(c[3],d,f);if(6===g[0]){var
t=g[3],u=g[1],m=ex(d,g[2]);if(m){var
j=m[1],n=k(i,f,u,j[1],j[2],j[3],t),o=n[2],p=n[1],h=b(c[3],d,o);if(6===h[0]){var
q=h[2],r=h[1],v=h[3],w=b(c[71],d,p),s=l(a(e[9],w),v),x=s[1],y=a(c[18],[0,r,q,s[2]]);return[0,a(c[19],[0,r,q,x]),y]}return[0,p,o]}return[0,i,f]}return[0,i,f]}var
f=b(c[3],d,j);if(6===f[0]){var
q=f[3],r=f[1],m=ex(d,f[2]);if(m){var
h=m[1],n=k(p,j,r,h[1],h[2],h[3],q),o=l(n[1],n[2]);return[0,[0,o[1],o[2]]]}return 0}return 0}function
f2(d,h,f){function
n(z,f){var
o=ex(d,f);if(o){var
j=o[1],h=j[3],p=j[2],A=j[1];if(b(c[52],d,h))var
q=b(c[71],d,h),k=q[1],r=q[3];else
var
I=[0,h,[0,a(c[9],1)]],k=0,r=a(c[21],I);var
s=n(k,r),w=s[1],B=s[2],m=a(e[17][1],w),C=a(c[9],m+1|0),D=b(c[i][1],m+1|0,h),E=[0,b(c[i][1],m+1|0,p),D,C,B],F=[0,i9(aw,A),E],G=a(c[21],F),H=[0,$([0,k,0,p]),0];return[0,b(e[18],w,H),G]}var
x=u(ah),J=v===x?ah[1]:l===x?a(t[2],ah):ah;if(g(c[a4],d,J,f)){var
y=u(bh),K=b(c[76],d,f)[2],L=v===y?bh[1]:l===y?a(t[2],bh):bh;return[0,0,dk(d,[0,L,K])]}var
M=a(c[9],1);return[0,[0,$([0,z,0,f]),0],M]}return n(h,f)}function
jd(q){function
d(d){var
w=a(k[66][4],d),x=a(k[66][5],d),h=a(k[66][6],d);function
y(b){var
d=u(bE),f=a(fx,b),i=v===d?bE[1]:l===d?a(t[2],bE):bE,e=g(c[a4],h,i,f);if(e)return e;var
j=aZ(b);return a(r[e6],j)}var
z=b(e[17][aY],y,w)[1],i=u(bh);function
A(d,s){var
w=d[3],x=d[2],y=d[1],h=a(bF,s),i=h[3],j=h[1],k=u(aw),z=v===k?aw[1]:l===k?a(t[2],aw):aw,m=a5(y,z),n=m[2],o=m[1],A=b(c[77],o,n)[2],p=[0,i,g(c[39],j,i,w)],B=[0,a(c[10],j),x],C=[0,n,b(e[19][5],p,B)],f=u(ae),D=a(c[21],C),q=v===f?ae[1]:l===f?a(t[2],ae):ae,r=[0,a(au[9],q),A],E=[0,a(c[26],r),p];return[0,o,D,a(c[21],E)]}var
B=v===i?bh[1]:l===i?a(t[2],bh):bh,j=a5(h,B),m=u(ah),C=j[2],D=j[1],E=v===m?ah[1]:l===m?a(t[2],ah):ah,n=a5(D,E),f=g(d_,A,[0,n[1],C,n[2]],z),p=f[2],F=f[3],G=o(aq[2],0,x,f[1],p)[1],H=L(s[bN],0,[0,q],p,[0,F],cw),I=a(k[64][1],G);return b(k[71][2],I,H)}return a(k[66][10],d)}function
je(m,d,l){var
f=b(c[82],d,l),h=f[2],n=L(ax[2],0,0,m,d,f[1]),o=a(e[17][1],h),j=[0,d],k=cM(j,g(c[91],d,o,n)[1]),p=k[3],q=k[1],r=a(e[17][9],h),s=b(c[i][4],r,p);return[0,j[1],s,q]}function
jf(f,d,m,N,$){var
aJ=em(N-1|0,m)[2],aK=a(C[1][1][3],aJ),aL=b(c[i][1],N,aK),aM=a(R[1],N),aN=b(c[5],d[1],aL),ab=b(cO[2],f,aN),y=ab[1],aO=ab[2],ac=a(A[28],y),h=ac[2],ad=ac[1],ae=b(e[17][x],ad[6],aO),af=ae[1],aP=b(e[18],ae[2],[0,aM,0]),aQ=a(_[5],[0,y,af]),q=o(_[65],f,d[1],1,aQ),aS=a(e[17][9],q),ag=b(e[17][15],c[8],af),ah=b(e[17][15],c[8],aP),u=0,D=ah,t=aS,B=0,S=0,z=0;for(;;){if(D){if(t){var
v=D[1],aT=t[2],aU=t[1],aV=D[2];if(b(c[44],d[1],v)){var
aW=b(r[37],d[1],v);if(b(e[17][26],aW,ag))var
w=0,I=0;else{var
aY=b(r[37],d[1],v);if(b(e[17][26],aY,u))var
w=0,I=0;else
var
ai=b(c[65],d[1],v),aZ=a(C[1][1][3],aU),a0=b(r[36],d[1],aZ),a1=1,a2=function(i){return function(f,c){if(c)try{var
g=b(e[17][7],i,f-1|0),h=a(H[2],g);return h}catch(a){a=G(a);if(a[1]!==fJ)if(a[1]!==bd)throw a;var
d=1}else
var
d=c;return d}}(B),a3=[0,ai],a4=g(O[2][15],a2,a0,a1)?[0,ai]:0,w=a4,I=a3}}else
var
w=0,I=0;var
aX=a(H[2],w)?z:z+1|0,u=[0,v,u],D=aV,t=aT,B=[0,w,B],S=[0,I,S],z=aX;continue}}else
if(!t){var
l=0,n=B,k=S,j=u,T=z;for(;;){if(n){var
aj=n[1];if(aj){if(k)if(j){var
l=[0,[0,aj[1]],l],n=n[2],k=k[2],j=j[2];continue}}else
if(k){var
ak=k[1];if(ak){if(j){var
am=j[2],an=k[2],V=ak[1],ao=n[2],a5=[0,[0,0,$],b(F[c7],V-1|0,m)],a6=0,a7=function(l,m){return function(f,i){var
j=a(C[1][1][3],i),k=a(c[9],m-f|0),h=1-g(r[37],d[1],k,j);return h?h:b(e[17][29],[0,f],l)}}(l,V);if(g(F[96],a7,a6,a5)){var
l=[0,[0,V],l],n=ao,k=an,j=am,T=T-1|0;continue}var
l=[0,0,l],n=ao,k=an,j=am;continue}}else
if(j){var
l=[0,0,l],n=n[2],k=k[2],j=j[2];continue}}}else
if(!k)if(!j){var
X=a(e[17][9],l),a9=aa(b(e[18],q,m)),a_=[0,a9,aa(b(e[18],q,m))],a$=function(i,g){var
j=i[2],e=i[1],l=e[1];if(g){var
m=g[1],n=a(c[9],1),o=Q(d[1],e,n),p=a(c[9],(m+h[6]|0)+1|0),q=Q(d[1],e,p),r=b(c[65],d[1],q),k=fR(f,d[1],l,r,0,o),s=k[2],t=al(0,f,[0,d[1]],k[1],e);return[0,t,al(0,f,[0,d[1]],j,s)]}return[0,e,j]},ap=g(e[17][18],a$,a_,X),aq=ap[2],ar=ap[1],ba=a(c[9],1),bb=Q(d[1],ar,ba),as=b(c[65],d[1],bb)-1|0,bc=a(e[9],aq),J=b(e[17][c5],(as+h[6]|0)+1|0,bc),be=a(e[8],aq),bf=b(e[17][c5],(as+h[6]|0)+1|0,be),bg=bH(-(h[6]+1|0)|0,bf),at=aR(0,f,d[1],m,bg,J),bh=0,bj=function(i,g,e){var
j=e[2],k=e[1];if(g){var
l=g[1],m=ek(d[1],j,[0,(h[6]+1|0)-i|0]),n=a(c[9],(l+h[6]|0)+1|0),o=Q(d[1],e,n),p=b(c[65],d[1],o),q=cJ(tm,f,d[1],p,m,k);return al(tn,f,[0,d[1]],q,e)}return e},s=o(F[91],bj,bh,X,ar),bk=a(c[9],1),bl=Q(d[1],s,bk),p=b(c[65],d[1],bl)-1|0,bm=aa(m),bn=a(e[8],bm),bo=bH(h[6]+1|0,bn),bp=b(e[18],q,m),bq=aR(0,f,d[1],bp,bo,m),K=al(to,f,[0,d[1]],s,bq),au=Q(d[1],K,$),br=a(e[7],s),av=b(e[17][x],p,br),M=av[1],aw=b(e[17][c7],h[6]+1|0,av[2]),bs=function(g){var
i=a(e[8],s),d=-h[6]|0,b=i;for(;;){if(b){var
f=b[1];if(0===f[0])if(g===f[1])return a(c[9],d);var
d=d+1|0,b=b[2];continue}return a(E[3],tp)}},bt=function(a){return a+1|0},bu=b(F[56],p,bt),bv=b(F[15],bs,bu),ax=a(e[17][9],bv);if(0===T)var
aA=au,az=0,ay=0;else
var
bX=a(e[8],K),bY=cH(d[1],bX,q),bZ=d[1],b0=function(a){return Q(bZ,K,a)},b1=b(e[17][15],b0,u),b2=[0,p+1|0,0,0,0],b3=function(b,k,j,i){var
e=b[4],f=b[3],g=b[2],d=b[1];return i?[0,d+1|0,di(a(c[9],(p+h[6]|0)+1|0),g),f,e]:[0,d+1|0,[0,k,g],[0,j,f],[0,a(c[9],d),e]]},Z=L(F[94],b3,b2,bY,b1,X),b4=Z[4],b5=Z[3],aE=cM(d,a(e[17][9],Z[2])),aF=aE[3],aG=aE[1],b6=a(e[17][9],b5),aH=b(c[i][4],b6,aF),b7=a(e[17][9],b4),b8=bi(f,d,aG,b(c[i][4],b7,aF),aH),b9=[0,0,b8,b(c[i][1],1,au)],b_=a(c[18],b9),aI=function(a){var
f=b(c[38],a,M),g=b(c[38],f,aw),h=Q(d[1],at,g),i=[0,h,b(e[18],ah,ax)];return b(U[55],d[1],i)},b$=aI(aH),aA=b_,az=[0,d0(f,d,aI(aG),b$),0],ay=1;var
bw=d[1],bx=function(a){return Q(bw,K,a)},by=b(e[17][15],bx,ag),bz=a(c[i][1],-((p+h[6]|0)+1|0)|0),bA=b(e[17][15],bz,by),bB=b(r[14],aA,M),aB=b(c[38],bB,aw),bC=a(c[5],d[1]),aC=b(e[17][15],bC,bA),bD=b(c[5],d[1],aB),bE=o(cO[22],y,[0,ad,h],aC,bD),bF=a(_[5],[0,y,aC]),aD=b(_[60],f,bF),Y=aa(m),bG=Y[3],bI=Y[1],bJ=bH(h[6]+1|0,Y[2]),bK=b(e[18],q,bI),bL=aR(0,f,d[1],bK,bJ,bG),bM=al(tq,f,[0,d[1]],s,bL),bN=aa(J),bO=a(e[8],bN),bP=aa(M),bQ=a(e[8],bP),bR=function(g){var
j=g[5],k=a(e[19][12],g[2]),l=b(e[19][15],c[8],k),m=b(e[19][15],c[8],j),n=c$(g[1]),o=[0,a(c[28],n),l],q=a(c[21],o),r=b(c[i][1],g[3],q),t=[0,r,a8(0,g[3])],u=[0,a(c[21],t),0],v=a(e[19][11],m),w=b(e[18],v,u),x=b(e[17][15],c[W],g[4]),y=a(e[17][9],w),z=d[1];function
A(a){return ce(z,a)}var
B=b(e[17][15],A,y),C=bH(g[3],bO),h=b(e[18],B,C),D=cH(d[1],h,M),E=bH(p,h),F=b(e[18],bQ,E),G=a(e[7],s),H=b(e[18],x,J),I=b(e[18],D,H),K=aR(0,f,d[1],I,F,G);return al(tr,f,[0,d[1]],K,bM)},bS=b(e[19][15],bR,aD),bT=function(a){return a[3]},bU=b(e[19][15],bT,aD),bV=function(e,d,b){return[0,a(c[8],e),d,b]},bW=o(e[19][59],bV,bE,bU,bS);return[0,J,aB,bW,p,at,b(e[18],ax,az),ay]}throw[0,P,tl]}}throw[0,P,tk]}}function
ts(d){function
e(e){var
r=a(bF,b(y[18],e,d)),t=r[2],F=r[3],n=a(y[2],e),G=a(y[8],e),h=b(c[3],n,F);if(6===h[0])var
x=h[3],o=f2(n,h[1],h[2]),p=o[2],q=o[1],A=[0,a(c[10],d),[0,p]],B=a(c[21],A),C=b(c[i][5],p,x),D=g(U[18],G,n,C),E=b(c[37],D,q),l=[0,[0,b(c[38],B,q),E]];else
var
l=0;if(l){var
u=l[1],v=u[2],w=u[1];if(t){var
H=b(c[i][9],[0,[0,d,t[1]],0],w),I=z(L(s[bN],0,[0,d],H,[0,v],cw)),J=z(a(s[75],[0,d,0]));return g(m[5],J,I,e)}var
K=a(y[38],w),M=b(s[lb],d,v),N=a(k[70][8],M);return a(b(m[9],N,K),e)}var
O=a(j[1][9],d),P=a(f[3],tt),Q=b(f[12],P,O);return g(m[24],0,Q,e)}return b(k[70][1],0,e)}function
tu(h){var
q=a(k[66][5],h),n=a(k[66][6],h),o=a(k[66][3],h),d=b(c[3],n,o);if(6===d[0]){var
r=d[2],s=d[1],P=d[3],w=function(h){var
m=f2(h,s,r),d=m[1],B=b(c[i][5],m[2],P),C=b(c[38],B,d),D=[0,C,V(0,d)],E=a(c[21],D),F=b(c[37],E,d);function
k(b){var
c=u(b),d=v===c?b[1]:l===c?a(t[2],b):b,e=a(j[x][3],d);return a(be[8][8],e)}var
w=[0,be[8][1],[0,be[8][4],0]],y=[0,k(ai),w],z=[0,k(aD),y],A=a(be[8][15],z),G=g(a(U[15],A),q,h,F);function
n(h,m,d){var
b=d[2],e=d[1];if(h)return[0,[0,b,e],b];var
f=u(aD),i=v===f?aD[1]:l===f?a(t[2],aD):aD,g=u(ai),j=a(c[24],[0,i,b]),k=v===g?ai[1]:l===g?a(t[2],ai):ai;return[0,[0,j,e],a(c[24],[0,k,b])]}if(d){var
o=d[2],H=d[1];if(o)var
I=[0,0,a(c[9],1)],J=0,K=function(a,b){return n(J,a,b)},f=n(1,H,g(e[17][19],K,o,I))[1];else{a(c[9],1);var
f=[0,a(c[9],1),0]}}else{a(c[9],1);var
f=[0,a(c[9],1),0]}var
p=cC(q,h,0,G),L=p[2],M=p[1],N=[0,L,a(bm[70],f)],O=[0,s,r,a(c[21],N)];return[0,M,a(c[19],O)]};return b(cf[2],1,w)}var
p=a(f[3],tv);return b(m[66][4],0,p)}var
tw=a(k[66][9],tu);function
tx(h,g){function
c(d){var
e=a(k[66][5],d),c=je(e,a(k[66][6],d),h),f=c[2],i=c[3],j=o(aq[2],0,e,c[1],f)[1],l=L(s[bN],0,[0,g],f,[0,i],cw),m=a(k[64][1],j);return b(k[71][2],m,l)}return a(k[66][10],c)}function
ty(e,g){function
d(d){var
h=a(k[66][5],d),i=a(k[66][6],d),f=jb(h,i,b(y[42][16],e,d)),j=f[3],l=f[1],m=[0,j,[0,a(c[10],e)]],n=a(c[21],m),o=L(s[bN],0,[0,g],n,0,cw),p=a(k[64][1],l);return b(k[71][2],p,o)}return a(k[66][10],d)}var
cP=[0,ts,tw,tx,function(d){function
c(c){var
e=a(k[66][5],c),f=a(k[66][6],c),g=a(fy,b(y[42][15],d,c));return jc(1,a(H[7],g),d,e,f)}return a(k[66][10],c)},ty];aH(1073,[0,cL,sS,sT,ew,ex,sV,cM,f0,i$,f1,jb,jc,tj,du,jd,f2,je,jf,cP],mg);function
jg(a){return aG(tz,0)}function
jh(cS,f,q,m){var
ar=bQ(f,m),y=a(A[28],ar),M=y[2][2],as=y[1],at=a(e[17][1],M),n=as[6],l=at-n|0,au=b(e[17][15],c[W],M),N=b(e[17][x],l,au),h=N[2],d=N[1],av=V(0,h),aw=[0,a(c[26],m),av],z=a(c[21],aw),ap=1;function
O(a){var
d=b(c[82],f,a)[2];return b(e[17][x],n,d)[2]}var
ax=bQ(f,m),ay=b(cO[18],ax,y);function
az(k,r){var
s=a(c[8],r),l=b(c[90],f,s),d=l[1],t=l[2],o=a(e[17][1],d),p=o-n|0,q=b(e[17][x],p,d)[1];function
u(d,k){var
l=a(ak,k)[3],e=b(c[90],f,l),g=e[2],n=e[1],o=b(c[82],f,g)[1],h=b(c[3],f,o);if(11===h[0])if(b(j[37],h[1][1],m[1])){var
p=O(b(c[i][1],d+1|0,g));return[0,[0,n,d,a(c[9],d+1|0),p]]}return 0}var
v=b(F[73],u,q),w=V(0,d),y=[0,a(c[29],[0,m,k+1|0]),w],z=a(c[21],y),A=O(t),B=1;function
C(j,f){var
g=f[1],l=f[4],m=f[3],n=f[2],d=a(e[17][1],g),r=[0,b(c[i][1],d,z),0],s=V(0,g),t=[0,b(c[i][1],d,m),s],u=[0,a(c[21],t),r],v=a(c[i][1],d),w=b(e[17][15],v,A),x=b(e[18],w,u),y=b(e[18],l,x),B=aQ(p+d|0,h),C=b(e[18],B,y),D=a(e[19][12],C),E=[0,a(c[9],(o+1|0)+d|0),D],F=a(c[21],E),G=a6(n+1|0,g),H=b(c[37],F,G);return[0,k,j,b(c[37],H,q)]}return g(e[17][77],C,B,v)}var
aA=b(e[19][16],az,ay),aB=0;function
aC(c,a){return b(e[18],c,a)}var
Q=g(e[19][18],aC,aA,aB),R=a6(l,d),aD=a6(l,R);function
C(e){var
f=V(e+an.caml_mul(2-e|0,l)|0,d),g=[0,b(c[i][1],(3*l|0)+e|0,z),f];return a(c[21],g)}var
aG=C(0),aH=[0,[0,[0,a(j[1][6],tA)],0,aG],0],aJ=C(1),aK=[0,[0,[0,a(j[1][6],tB)],0,aJ],aH],aL=C(2),aM=[0,[0,[0,a(j[1][6],tC)],0,aL],aK],aN=b(e[18],R,d),aP=b(e[18],aD,aN),aR=h4(aM),p=3*(l+1|0)|0,aS=b(e[18],aR,aP),aU=[0,a(c[9],2),0],aV=[0,a(c[9],3),aU],aW=aQ(l+3|0,d),aX=b(e[18],aW,aV),aY=aQ((2*l|0)+3|0,d),aZ=b(e[18],aY,aX),a0=aQ(p,h),a1=b(e[18],a0,aZ),a2=a(e[19][12],a1),a3=[0,a(c[9],(p+1|0)+n|0),a2],a4=a(c[21],a3),a5=[0,a(c[9],1),0],a7=[0,a(c[9],2),a5],a8=aQ(3,d),a9=b(e[18],a8,a7),a$=aQ(l+3|0,d),ba=b(e[18],a$,a9),bb=aQ(p,h),bc=b(e[18],bb,ba),bd=a(e[19][12],bc),be=[0,a(c[9],(p+1|0)+n|0),bd],bf=a(c[21],be),bg=[0,a(c[9],1),0],bh=[0,a(c[9],3),bg],bi=aQ(3,d),bj=b(e[18],bi,bh),bk=aQ((2*l|0)+3|0,d),bl=b(e[18],bk,bj),bm=aQ(p,h),bn=b(e[18],bm,bl),bo=a(e[19][12],bn),bp=[0,a(c[9],(p+1|0)+n|0),bo],bq=a(c[21],bp),bs=b(c[i][1],2,bq),bt=[0,0,b(c[i][1],1,bf),bs],bu=[0,0,a4,a(c[18],bt)],bv=a(c[18],bu);b(c[37],bv,aS);var
bR=b(w[mc],q,f),bx=a(ao[41],[2,m[1]]),S=b(K[5],bx,tD),bS=0;function
by(a){return b(c[5],f,a[3])}var
bz=b(e[17][15],by,Q);function
bA(c){var
d=c[1],e=a(E[22],c[2]),f=b(E[17],tE,e),g=a(E[22],d),h=b(E[17],g,f),i=b(E[17],tF,h);return b(K[5],S,i)}var
bB=b(e[17][15],bA,Q),D=a(e[17][1],d),bC=a6(D,d),bD=b(e[18],bC,d),bE=V(2*D|0,h),bF=[0,a(c[26],m),bE],T=a(c[21],bF),bG=c[14],bH=[0,T,V(0,d)],bI=a(c[21],bH),bJ=[0,0,b(c[i][1],1,bI),bG],bK=a(c[18],bJ),bM=[0,T,V(D,d)],bN=[0,0,a(c[21],bM),bK],bO=a(c[18],bN),bP=b(c[37],bO,bD),U=[0,[0,S,b(c[5],f,bP),0,bB,bz],bS],bT=0;function
bU(h){var
d=a(ak,h),e=d[2],g=d[1],i=d[3];if(e){var
j=[0,b(c[5],f,e[1])];return[0,a(K[10][16],g),j]}var
k=[1,b(c[5],f,i)];return[0,a(K[10][16],g),k]}var
bW=[0,0,0,b(e[17][15],bU,h),U,bR,bT],G=g(ji[2],bW,aO[8],0),I=a(A[2],0),bX=a(w[17],I),X=o(w[lB],0,I,bX,[0,G,0]),bY=X[1];f1(I,bY,q,c$(X[2]));var
Y=a(c[25],[0,G,0]),bZ=0;function
b0(b,a){var
c=a[5],d=1;function
f(a,c){return[0,d7,q,1,0,[0,[3,[0,[0,G,b],a]]]]}return g(e[17][77],f,d,c)}var
b1=g(e[17][77],b0,bZ,U),b2=[0,a(e[17][12],b1)];g(aI[22],0,[0,fr,0],b2);var
b3=a(ao[41],[2,m[1]]),Z=b(K[5],b3,tG),_=b(K[6],tH,Z),k=[0,f],u=a(A[2],0),$=dd(f,hs(k));if(a(e[17][55],d))var
b4=[0,Y,V(0,h)],s=h,r=z,aa=a(c[21],b4);else
var
t=f0(0,u,k,h,z),L=t[8],ah=t[6],ai=t[5],aj=t[3],ct=t[4],al=b(c[x],aj,u),cu=b(c[i][2],2,2),am=b(e[17][15],cu,ct),cv=[0,ai,a(c[9],1)],cw=a(c[24],cv),cx=a(c[i][5],cw),cy=b(e[17][15],cx,am),cz=[0,ai,a(c[9],2)],cA=a(c[24],cz),cB=a(c[i][5],cA),cC=b(e[17][15],cB,am),cD=[0,ah,a(c[9],1)],cE=[0,a(c[24],cD),0],cF=[0,ah,a(c[9],2)],cG=[0,a(c[24],cF),cE],cH=b(e[18],cy,cG),cI=b(e[18],cC,cH),cJ=aQ(2,h),cK=aE(Y,b(e[18],cJ,cI)),cL=b(c[i][1],1,L),cM=[0,[0,a(j[1][6],tK)],cL,cK],cN=a(c[19],cM),cP=[0,[0,a(j[1][6],tL)],L,cN],cQ=a(c[19],cP),cR=g(a_[9],al,k[1],L),s=aj,r=cR,aa=g(a_[9],al,k[1],cQ);var
b5=[0,hv(k),[0,r,aa]],b6=a(c[21],b5),b7=b(c[38],b6,s),b8=[0,hu(k),[0,r]],b9=a(c[21],b8),b_=[0,b(c[37],b9,h)],ab=bw(Z,b7,b_,q,k[1],tI);g(aI[22],0,[0,fr,0],[3,[0,[1,ab],0]]);var
b$=V(0,h),ca=[0,a(c[22],ab),b$],ac=a(c[21],ca),cb=b(c[x],s,u),cc=[0,ht(k),[0,r,ac]],cd=a(c[21],cc),ce=[0,r,[0,ac,[0,ck(B[7],cb,k,0,0,0,0,0,0,cd),0]]],ae=fC(k[1],$,ce),cf=ae[1],af=b(c[37],ae[2],s),cg=a(H[7],cf),ag=b(c[38],cg,s);function
ch(e,b,d){if(1===b[0]){var
c=o(ad[5],$[1],d7,ap,[1,b[1]]);return a(ad[6],c)}throw[0,P,tJ]}var
ci=a(A[2],0);o(aq[3],0,ci,k,ag);var
cj=a(A[2],0);o(aq[3],0,cj,k,af);var
v=a(B[47],k[1])[1],cl=b(c[5],v,af),cm=b(c[5],v,ag),J=aF(aT[5],u,_,v,0,0,cm,cl),cn=J[4],co=J[3],cp=J[1],cq=a(w[br],v),cr=[0,a(bV[1],ch)],cs=[0,jg(0)];bL(aT[7],_,[0,co],cn,cq,0,0,[0,[0,2,q,10]],cs,0,cr,0,cp);return 0}cb([0,tM,function(a,b){return ca(jh,a,b)}]);function
jj(u,T,t,s){var
h=s[1],d=[0,T],U=s[2],z=a(A[27],h),p=z[2],v=z[1],n=v[6],C=p[6],k=p[7],D=b(e[17][15],c[W],p[2]),X=V(0,D),Y=[0,a(c[26],s),X],Z=a(c[21],Y),E=[0,$([0,[0,a(j[1][6],tN)],0,Z]),D],G=b(e[17][x],k+1|0,E),I=G[2],l=G[1],J=g(B[11],[0,w[c2]],u,d),L=b(c[37],J,l),aa=b(c[38],J,l),ab=b(c[i][1],k+1|0,aa),ac=a8(k+1|0,n),M=V(0,b(F[c7],k+1|0,E)),f=a(j[1][6],tO),N=[0,[0,f],L],ad=b(c[i][1],1,L),y=a(j[1][6],tP),m=a(j[1][6],tQ),q=a(j[1][6],tR),ae=[0,a(c[10],f)],af=b(e[19][5],ac,ae),O=b(e[19][5],af,M),ag=[0,a(c[10],q),O],P=a(c[21],ag),ah=b(c[37],P,l),ai=b(c[i][1],2,ah),aj=b(c[38],P,l),ak=b(c[i][1],k+1|0,aj),al=p[9];function
am(C,B){var
j=a(c[8],B),l=a(_[47],[0,h,C+1|0]),p=a(c[10],m),D=b(c[2][2],d[1],U),E=g(cO[5],h[1],v,D),F=b(e[17][15],c[8],E),G=b(c[i][4],F,j),s=b(c[90],d[1],G)[1],I=a(e[17][1],s)-n|0,t=b(e[17][x],I,s)[1],J=v[4]-h[2]|0,K=a8(-n|0,n),L=[0,a(c[9],J),K],M=a(c[21],L),N=o(r[51],d[1],M,p,j),u=b(c[90],d[1],N)[1],O=a(e[17][1],u)-n|0,P=b(e[17][x],O,u)[1];function
Q(e,f){var
d=e[2],g=e[1],h=a(bG,f),j=b(c[i][1],d,h);return[0,[0,[0,a(c[9],d),j],g],d+1|0]}var
w=g(e[17][18],Q,tS,P)[1];function
z(f,h){var
i=0;function
j(e,h){if(e){var
i=e[1],j=i[2],k=i[1],l=b(f,function(b){var
e=b[2],f=b[1],g=[0,dW(d),[0,e,j]],h=a(c[21],g),i=[0,fl(d),[0,e,j,f,k]];return[0,a(c[21],i),h]},h),m=function(a){return[0,a]};return g(H[24],m,e,l)}return b(f,function(a){return a},h)}var
k=g(e[17][18],j,i,h),l=ar(d,fn(0)),m=[0,ar(d,hm(0)),l];function
n(a){return a}return g(H[24],n,m,k)}function
A(o,n,j){var
q=j[1],k=b(c[90],d[1],j[2]),f=k[1],l=b(c[82],d[1],k[2]),r=l[2];if(g(c[94],d[1],l[1],p)){var
h=a(e[17][1],f),s=a8(0,h),t=[0,b(c[i][1],h,q),s],u=[0,a(c[21],t),0],v=b(e[18],r,u),m=b(o,a(e[19][12],v),h),w=m[2],x=b(c[38],m[1],f);return[0,a(n,[0,x,b(c[37],w,f)])]}return 0}function
R(b,l){var
e=[0,a(c[10],m),b],g=a(c[21],e),h=[0,a(c[10],f),b],i=[0,a(c[21],h),g],j=[0,dW(d),i],k=a(c[21],j);return[0,a(c[9],0),k]}var
S=z(function(a,b){return A(R,a,b)},w)[2];function
T(g,j){var
k=[0,a(c[10],m),g],h=a(c[21],k),o=[0,a(c[10],f)],p=b(e[19][5],o,g),r=a8(l+j|0,n),i=b(e[19][5],r,p),s=[0,a(c[10],y),g],t=[0,a(c[21],s),[0,h]],u=a(c[21],t),v=[0,a(c[10],q),i],w=a(c[21],v),x=[0,a(c[10],f),g],z=[0,a(c[21],x),w,u,h],A=[0,fl(d),z],B=a(c[21],A),C=[0,a(c[10],q),i],D=a(c[21],C),E=[0,a(c[10],f),g],F=[0,a(c[21],E),D],G=[0,dW(d),F];return[0,B,a(c[21],G)]}var
V=z(function(a,b){return A(T,a,b)},w)[1],W=b(c[38],S,t),X=b(c[i][1],k+1|0,W),Y=b(c[38],V,t);return[0,l,X,b(c[i][1],k+1|0,Y)]}var
Q=b(e[19][16],am,al),an=b(e[19][15],e[8],Q),ap=a(c[9],1),aq=[0,g(_[76],u,h,4),ab,ap,an],as=a(c[30],aq),at=b(e[19][15],e[9],Q),au=a(c[9],1),av=[0,g(_[76],u,h,4),ak,au,at],aw=a(c[30],av),ax=b(c[38],aw,l),ay=b(c[i][1],3,ax),az=b(c[38],as,l),aA=b(c[i][1],2,az),aB=[0,[0,[0,C],0],[0,[0,[0,m]],[0,ad],[0,b(c[i][11],[0,m,[0,f,0]],aA)]]],aC=a(c[31],aB),aD=b(c[38],aC,[0,N,I]),aE=a(ao[41],[2,h]),aF=b(K[6],tT,aE),R=bw(aF,aD,0,t,d[1],tU),aG=[0,[0,[0,C],0],[0,[0,[0,m]],[0,ai],[0,b(c[i][11],[0,m,[0,y,0]],ay)]]],aH=a(c[31],aG),aI=a(c[i][1],1),aJ=b(e[19][15],aI,M),aK=[0,a(c[10],f),aJ],aL=a(c[21],aK),aM=[0,a(c[22],R),O],aN=[0,0,a(c[21],aM),aL],aO=a(c[18],aN),aP=b(c[37],aO,l),aQ=[0,[0,y],b(c[i][1],1,aP)],aR=b(c[36],aQ,aH),aS=b(c[i][11],[0,f,0],aR),aT=b(c[38],aS,[0,N,I]),aU=[0,[0,q,a(c[22],R)],0],aV=b(c[i][9],aU,aT),aW=a(ao[41],[2,h]),aX=b(K[6],tV,aW);if(t)var
S=d[1];else
var
aY=a(A[2],0),S=a(w[17],aY);bw(aX,aV,0,t,S,tW);return 0}cb([0,tX,function(a,b){return ca(jj,a,b)}]);aH(1077,[0,jg,jh,jj],gH);var
jk=a(e[17][15],c[W]);function
jl(d,l){var
m=l[2],n=l[1],f=n[1],o=a(A[27],n)[1],q=o[8],r=b(c[2][2],d,m),s=a(jk,b(a1[25],r,q)),h=cA(0,function(b){return a(j[1][6],tY)},s),k=h[3],p=h[1],t=h[2],u=a(e[17][1],k),v=a(A[2],0),w=g(e[17][19],c[c2],k,v);function
y(h,j){var
k=[0,[0,f,h],m],l=d8(0,p,a(jk,b(e[17][x],j[6],j[2])[1])),n=a(ao[41],[2,[0,f,h]]),o=[0,a(c[26],k),t],q=a(c[34],o),r=bQ(d,k),s=b(_[4],w,r);function
v(e){var
f=a(c[8],e),h=g(c[91],d,u,f)[2],j=b(c[i][4],p,h);return b(c[90],d,j)}var
y=b(e[19][15],v,s);return[0,n,q,l,y,function(e,d,b){var
i=a(A[2],0),j=[0,g(_[76],i,[0,f,h],4),d,e,b];return a(c[30],j)}]}return[0,k,b(e[19][16],y,o[1])]}function
jm(c){var
d=as(t0,tZ,c),e=b(ad[11],c[1],d);return a(H[7],e)}function
jn(a){return as(t2,t1,a)}function
jo(d){function
f(b){var
d=aZ(b);return a(c[10],d)}var
g=b(e[17][15],f,d);return a(e[19][12],g)}function
jp(u,f,n,m){var
h=jl(f,m),g=h[1],d=[0,f],k=jm(d)[2][1],p=a(e[19][11],h[2]);function
q(f){var
l=V(0,f[3]),h=a(c[21],[0,f[2],l]),m=[0,jn(d),[0,h]],o=a(c[21],m),p=[0,a(j[1][6],t3)],q=[0,a(j[1][6],t4)],s=a(c[9],1),t=[0,a(c[9],2),s],u=[0,b(c[i][1],2,o),t],v=a(c[21],u),x=[0,q,b(c[i][1],1,h),v],y=[0,p,h,a(c[18],x)],z=a(c[18],y),B=b(c[37],z,f[3]),C=b(r[17],B,g);return[0,f,[0,C,function(j){var
m=jo(g),o=b(e[19][5],m,l),p=[0,h,[0,b7(a(A[2],0),d,j,o),0]],i=b(ad[16],k,p),q=i[2],s=f[3],t=a(H[7],i[1]),u=b(c[38],t,s),v=b(r[19],u,g),x=b(w[c_],n,d[1]),y=b(c[37],q,f[3]),z=b(r[17],y,g),B=[0,b(c[5],d[1],z)],C=t5[6],D=aP[40][1],E=[0,[0,b(c[5],d[1],v),D],C];return[0,b(t6[6],0,E),0,0,B,x,0,0]}]]}var
l=b(e[17][15],q,p);function
s(g,d,f){function
c(c){var
e=c[1],f=[0,[0,a(c[2][2],d)],t7],g=b(K[5],e[1],t8),h=[1,L(bg[3],0,0,g,0,f)],i=o(ad[5],k[1],aI[4],1,h);return a(ad[6],i)}return b(e[17][14],c,l)}function
t(e){var
f=e[2][1],g=b(K[5],e[1][1],t9),h=[0,a(bV[1],s)],i=[0,hN(0)],j=a(w[br],d[1]),k=b(c[5],d[1],f);bL(aT[7],g,0,k,j,0,0,0,i,0,h,0,[0]);return 0}return b(e[17][14],t,l)}cb([0,t_,function(a,b){return ca(jp,a,b)}]);aH(1081,[0,jl,jm,jn,jo,jp],lz);function
f3(h,d,k,r,q){var
l=ed(h,d,L(ax[2],0,0,h,d,k))[1],m=dl(l),f=m[1],s=m[2],t=f[1][1],n=a(A[27],f[1]),j=n[2],o=n[1],u=o[1];function
v(b,d){return a(c[26],[0,[0,t,b],f[2]])}var
w=b(e[19][16],v,u),y=a(e[19][11],w),z=a(e[17][9],y);a(e[17][1],j[2]);var
p=o[6],B=g(_[76],h,f[1],4),C=j[9],D=j[4];function
E(n,m,l){var
o=a(c[8],l),r=b(c[i][4],z,o),g=b(c[90],d,r),h=g[1],t=g[2],u=a(e[17][1],h)-p|0,v=b(e[17][x],u,h)[1],w=b(c[37],t,v),y=a(e[17][9],s),A=b(c[i][4],y,w),j=b(c[90],d,A),k=j[1],B=c0(q,f,n,m,p,k,j[2]);return b(c[38],B,k)}var
F=g(e[19][58],E,D,C);return aF(_[77],h,d,l,B,r,k,F)}function
jq(n,as,ap,m){var
N=m[1],d=[0,as],at=m[2],O=a(A[27],N),Q=O[2],au=O[1],av=Q[2],aw=b(c[2][2],d[1],at),ay=b(a1[25],aw,av),az=b(e[17][15],c[W],ay),R=ec(d[1],az),aA=a(e[17][1],R),aB=au[6],aC=Q[6],S=a8(0,aA),T=b(e[19][54],aB,S)[2];if(0===T.length-1)var
aD=a(c[9],1),aE=[0,a(c[26],m),S],q=a(c[21],aE),s=aD,h=R,p=0;else{var
k=du(n,d[1],m),al=k[3],bo=k[8],bp=k[7],bq=k[6],bs=k[2];d[1]=k[1];var
am=u(ae),bt=v===am?ae[1]:l===am?a(t[2],ae):ae,bu=b(B[14],d,bt),bv=b(c[5],d[1],bs),bw=a(e[17][1],al),bx=b(b$[81],bw,bv)[2],by=[0,bu,[0,bo,a(c[8],bx)]],bz=a(c[21],by),bA=b(e[17][c7],bp,bq),an=u(ai),bB=a(c[9],1),bC=v===an?ai[1]:l===an?a(t[2],ai):ai,bD=a(c[24],[0,bC,bB]),q=g(U[19],n,d[1],bz),s=bD,h=al,p=bA}var
aG=ar(d,fn(0)),aH=ar(d,hn(0)),V=a(j[1][6],t$),y=a(j[1][6],ua),z=[0,$([0,[0,V],0,q]),h],aI=[0,$([0,[0,y],0,b(c[i][1],1,q)]),z],aJ=cu(0),aK=g(w[l4],0,0,n),aL=g(B[65],aK,d,aJ),D=a(c[13],aL),aM=b(c[37],D,aI),X=b(c[x],z,n),Y=g(C[1][14],c[9],0,h);function
Z(a){return b(c[i][1],a,q)}function
_(d){var
f=a(c[i][1],d),g=b(e[19][15],f,Y),h=b(e[19][5],g,T),j=[0,a(c[26],m),h];return a(c[21],j)}var
aa=a(e[17][1],p),aN=Z(aC+2|0),aO=a6(1,p),aP=[0,$([0,[0,V],0,_(aa+1|0)]),aO],aQ=$([0,0,0,aN]),aR=b(c[35],aQ,D),aS=b(c[38],aR,aP);function
aU(w,o,v,u,f,t){var
g=[0,[0,y],0,Z(a(e[17][1],f)+1|0)],j=[0,$(g),f],h=b(c[x],j,X),k=[0,[0,y],0,_((a(e[17][1],f)+aa|0)+2|0)];function
l(t,p,s,r,j,q){if(o===p){if(0===a(e[17][1],f))return aG;var
k=b(c[x],j,h),l=cM(d,f)[3],m=cM(d,j)[3],n=a(e[17][1],f)+1|0,g=b(c[i][1],n,l);return bi(k,d,L(ax[2],0,0,k,d[1],g),g,m)}return aH}var
m=[0,$(k),p],n=b(c[38],D,m),q=f3(h,d[1],s,n,l),r=$(g);return b(c[36],r,q)}var
aV=f3(X,d[1],s,aS,aU),aW=dV(0,[0,ap],d,[0,aM],b(c[38],aV,z)),E=a(ao[41],[2,N]),aX=b(K[6],ub,E),aY=b(K[6],uc,E),aZ=b(K[6],ud,E),a0=L(bg[3],0,0,aX,0,[0,[0,aW],ue]),ab=a(A[2],0),f=[0,a(w[17],ab)],ac=u(de),a2=v===ac?de[1]:l===ac?a(t[2],de):de,F=a(ad[8],a2),a3=b(B[14],f,[1,a0]),a4=b(B[14],f,F[2]),a5=b(c[76],f[1],a4)[2],af=a(c[21],[0,a3,Y]),a7=L(ax[2],0,0,ab,f[1],af),ag=b(c[3],f[1],a7);if(6===ag[0]){var
ah=fC(f[1],[0,F,a5],[0,ag[2],[0,af,0]])[1],a9=a(A[2],0),r=b(c[x],h,a9),ba=a(H[7],ah),bb=L(ax[2],0,0,r,f[1],ba),I=a(H[7],ah),G=bb;for(;;){var
J=b(c[3],f[1],G);if(6===J[0]){var
a_=J[3],aj=ck(B[7],r,f,0,0,0,0,0,0,J[2]),a$=b(c[i][5],aj,a_),I=a(c[21],[0,I,[0,aj]]),G=a$;continue}var
ak=b(c[38],I,h),bc=b(c[37],G,h);o(aq[3],0,r,f,ak);var
bd=function(e,b,d){var
c=o(ad[5],F,d7,1,b);return a(ad[6],c)},be=b(c[5],f[1],bc),bf=b(c[5],f[1],ak),M=aF(aT[5],r,aY,f[1],0,0,bf,be),bh=M[4],bj=M[3],bk=M[1],bl=[0,a(bV[1],bd)],bm=[0,hM(0)],bn=a(w[br],f[1]);bL(aT[7],aZ,[0,bj],bh,bn,0,0,0,bm,0,bl,0,bk);return 0}}throw[0,P,uf]}cb([0,ug,function(a,b){return ca(jq,a,b)}]);aH(1082,[0,f3,jq],lR);var
f4=a(aC[1],uh);function
ey(d,c){return[l,function(f){var
e=b(f4,d,c);return a(au[9],e)}]}function
f5(d,c){return[l,function(f){var
e=b(f4,d,c);return a(au[8],e)}]}function
aU(a){return f5(ui,a)}var
dx=[l,function(c){var
b=a(aC[39],0);return a(au[9],b)}],uj=[l,function(c){var
b=a(aC[40],0);return a(au[10],b)}],um=f5(ul,uk),bW=ey(uo,un),bX=ey(uq,up),ez=aU(ur),uu=f5(ut,us),uw=aU(uv),uz=ey(uy,ux),eA=aU(uA),eB=aU(uB),eC=aU(uC),bY=aU(uD),eD=aU(uE),eE=aU(uF),eF=aU(uG),eG=aU(uH),eH=aU(uI),uK=aU(uJ),uM=aU(uL),uO=aU(uN),uQ=aU(uP),dy=ey(uS,uR),dw=[l,function(d){var
c=b(f4,uU,uT);return a(au[10],c)}];function
f6(c,e){var
d=u(c),f=v===d?c[1]:l===d?a(t[2],c):c;return b(B[14],e,[2,f])}function
cg(c,e){var
d=u(c),f=v===d?c[1]:l===d?a(t[2],c):c;return b(B[14],e,[1,f])}function
jr(c,e){var
d=u(c),f=v===d?c[1]:l===d?a(t[2],c):c;return b(B[14],e,[3,f])}function
uV(a){return f6(dy,a)}function
uW(a){return jr(dw,a)}function
uX(a){return f6(dx,a)}function
js(a){return jr(uj,a)}function
jt(a){return cg(um,a)}function
uY(a){return f6(uz,a)}function
uZ(a){return cg(uK,a)}function
u0(a){return cg(uM,a)}function
u1(a){return cg(uO,a)}function
u2(a){return cg(uQ,a)}var
ab=[e9,u3,e4(0)];function
dz(f,d,l,e,k){var
g=e[2],h=e[1],m=l[1],n=b(c[x],h,f),i=ck(B[7],n,d,0,0,0,0,0,0,g),j=a(k,i),p=b(c[x],m,f);o(aq[3],0,p,d,j);return[0,[0,[0,[0,h,g],b(c[75],d[1],i)]],j]}function
ch(d,e,v,u,h,n){var
x=v[1];switch(h[0]){case
0:throw[0,P,u4];case
1:var
y=h[1],z=b(w[168],0,d),o=g(B[65],z,e,y),A=a(R[16],o),j=[0,A,b(u5[30],d,o)];break;case
2:var
L=h[1],M=b(w[lB],0,d),s=g(B[65],M,e,L),N=a(R[19],s),j=[0,N,b(_[1],d,s)];break;default:var
O=h[1],Q=b(w[170],0,d),t=g(B[65],Q,e,O),S=a(R[21],t),j=[0,S,b(_[2],d,t)]}var
C=j[2],J=a(c[8],j[1]),q=a(c[8],C),l=q,k=n;for(;;){var
f=b(c[3],e[1],l);switch(f[0]){case
5:var
l=f[1];continue;case
6:if(k){var
p=k[1],D=f[3],G=f[2];if(p){var
I=k[2],l=b(c[i][5],p[1],D),k=I;continue}var
r=G,m=1}else
var
m=0;break;case
8:var
l=b(c[i][5],f[2],f[4]);continue;default:var
m=0}if(!m)var
r=a(E[3],u6);var
K=b(U[24],e[1],r);return dz(d,e,[0,x,q],[0,u,K],function(d){var
e=a(H[25],d),f=b(F[15],e,n),g=[0,J,a(aK[12],f)];return a(c[21],g)})}}var
u7=a(eI[13],j[61]);function
f7(g,f,e,d,a){var
h=b(c[x],e,g);return aF(U[83],0,0,0,h,f,d,a)[2]}function
ju(B,d,h,f){var
j=h[1],m=f[2],n=f[1],q=h[2];if(j){var
k=j[1],e=k[2][1],r=k[1][1],s=b(w[23],d[1],e),t=a(w[6],s),u=a(p[1],r),v=b(F[c7],u,t),x=function(b){var
d=a(C[2][1][1],b);return a(c[10],d)},y=b(p[17],x,v),l=b(c[i][4],y,m),z=d[1],A=b(c[5],d[1],l);d[1]=g(w[31],e,A,z);d[1]=o(dZ[16],d[1],e,l,u7);return[0,n,q]}throw[0,P,u8]}function
jv(d,c,b,a){var
e=a[2],f=b[2],g=ju(d,c,b[1],a[1]);return[0,g,al(0,d,[0,c[1]],e,f)]}function
u9(j,e,d,a){var
f=a[2],h=a[1],i=g(j,e,d,[0,h,f]),k=i[1][2],l=b(c[x],h,e);o(aq[5],l,d,k,f);return i}function
cQ(h,f,b,a,e){var
c=g(f,b,a,e),d=c[1][1];return d?jv(b,a,c,g(h,b,a,d[1][1])):c}function
cR(c,b,a){var
d=aa(a[1]);return[0,dz(c,b,a,a,function(a){return a}),d]}function
u_(h,d,c,b){function
e(c,b,i){var
a=g(h,c,b,i),f=a[1][1];if(f){var
j=f[1][1];try{var
d=[0,b[1]],k=jv(c,d,a,e(c,d,j));b[1]=d[1];return k}catch(b){b=G(b);if(b[1]===ab)return a;throw b}}return a}try{var
a=e(d,c,b);return a}catch(a){a=G(a);if(a[1]===ab)return cR(d,c,b);throw a}}function
cS(d,a){return b(c[a4],d,[2,a])}function
jw(d,a){return b(c[a4],d,[1,a])}function
bn(g,e){try{var
d=b(c[70],g,e)}catch(b){b=G(b);if(b===R[54])throw[0,ab,a(f[3],u$)];throw b}return[0,d[1],d[2],d[3]]}function
bZ(w,d,s,k,j,i,r){var
m=k?k[1]:0,n=j?j[1]:0,o=i?i[1]:0,p=cD(d,r),e=p[2],q=u(dx),x=p[1],y=v===q?dx[1]:l===q?a(t[2],dx):dx;if(1-a(cS(d,y),x))throw[0,ab,a(f[3],va)];var
z=T(e,0)[1],g=T(e,2)[3],h=T(e,1)[2],A=m?1-f7(w,d,s,h,g):m;if(A)throw[0,ab,a(f[3],vb)];var
B=n?1-b(c[44],d,h):n;if(B)throw[0,ab,a(f[3],vc)];var
C=o?1-b(c[44],d,g):o;if(C)throw[0,ab,a(f[3],vd)];return[0,z,h,g]}function
f8(e,h){var
f=cD(e,h),d=f[2],g=u(dw),i=f[1],j=v===g?dw[1]:l===g?a(t[2],dw):dw;if(a(b(c[a4],e,[3,j]),i)){var
k=T(d,3)[4],m=T(d,2)[3],n=T(d,1)[2];return[0,[0,T(d,0)[1],n,m,k]]}return 0}function
ve(m,e,d,h){var
n=h[2],f=h[1];try{var
s=[0,d[1]],H=g(m,e,s,[0,f,n]);d[1]=s[1];return H}catch(h){h=G(h);if(h[1]===ab){var
i=function(a){var
h=b(c[x],f,e);return g(a_[11],h,d[1],a)},o=i(n);try{var
j=b(c[70],d[1],o),t=j[3],u=j[1],v=[0,u,i(j[2]),t],q=a(c[18],v);try{var
k=bn(d[1],q),w=k[3],y=k[1],l=bZ(e,d[1],f,0,0,0,k[2]),z=l[3],A=l[1],B=i(l[2]),C=[0,A,B,i(z)],D=[0,uX(d),C],E=[0,y,a(c[21],D),w],F=a(c[18],E),r=F}catch(a){a=G(a);if(a[1]!==ab)throw a;var
r=q}var
p=r}catch(a){a=G(a);if(a!==R[54])throw a;var
p=o}return g(m,e,d,[0,f,p])}throw h}}function
vf(z,d,y){var
A=y[2],e=y[1],k=bn(d[1],A),h=k[3],B=k[2],L=k[1],C=bZ(z,d[1],e,0,0,0,B),M=C[3],D=f8(d[1],C[2]),E=f8(d[1],M);if(D)if(E){var
F=E[1],m=F[4],n=F[3],j=D[1],o=j[4],p=j[3],q=j[2],s=j[1];if(g(c[i][13],d[1],1,h))try{var
I=b(c[71],d[1],q)[3];if(!g(c[i][13],d[1],1,I))throw R[54];var
J=u(eD),P=v===J?eD[1]:l===J?a(t[2],eD):eD,S=a(r[47],I),Q=[1,P],T=[0,[0,s],[0,[0,S],[0,[0,a(r[47],h)],[0,[0,p],[0,[0,n],[0,[0,o],[0,[0,m],vh]]]]]]],x=Q,w=T}catch(b){b=G(b);if(b!==R[54])throw b;var
H=u(eE),N=v===H?eE[1]:l===H?a(t[2],eE):eE,x=[1,N],w=[0,[0,s],[0,[0,q],[0,[0,a(r[47],h)],[0,[0,p],[0,[0,n],[0,[0,o],[0,[0,m],vg]]]]]]]}else
var
K=u(eF),U=v===K?eF[1]:l===K?a(t[2],eF):eF,x=[1,U],w=[0,[0,s],[0,[0,q],[0,[0,p],[0,[0,n],[0,[0,o],[0,[0,m],[0,[0,a(c[19],[0,L,B,h])],vi]]]]]]];var
O=aa(e);return[0,ch(z,d,[0,e,A],e,x,w),O]}throw[0,ab,a(f[3],vj)]}function
vk(a,b,c){return u_(vf,a,b,c)}function
jx(D,h,e,q){var
j=q[2],d=q[1],m=bn(e[1],j),n=m[3],o=m[2],s=m[1],w=bZ(h,e[1],d,vl,0,0,o),y=w[2],k=w[1],p=aa(d);if(g(c[i][13],e[1],1,n)){var
E=function(d){var
e=[0,s,o,b(c[i][1],1,d)];return a(c[19],e)};return[0,dz(h,e,[0,d,j],[0,d,a(r[47],n)],E),p]}var
z=a(c[19],[0,s,o,n]);try{var
C=u(eH),K=v===C?eH[1]:l===C?a(t[2],eH):eH,L=[0,jt(e),[0,k]],M=a(c[21],L),N=b(c[x],d,h),O=b(ad[30],0,N),P=[0,ch(h,e,[0,d,j],d,[1,K],[0,[0,k],[0,[0,g(B[65],O,e,M)],[0,[0,y],[0,[0,z],vo]]]]),p];return P}catch(i){i=G(i);if(i===X){if(!D)if(!dS[1]){var
H=b(c[x],d,h),I=g(r[W],H,e[1],k),J=a(f[3],vn);throw[0,ab,b(f[12],J,I)]}var
A=u(eG),F=v===A?eG[1]:l===A?a(t[2],eG):eG;return[0,ch(h,e,[0,d,j],d,[1,F],[0,[0,k],[0,[0,y],[0,[0,z],vm]]]),p]}throw i}}function
jy(X,k,d,B){var
q=B[2],n=B[1],o=0===X?1:0,s=bn(d[1],q),D=s[2],E=s[1],Y=s[3],t=bZ(k,d[1],n,0,[0,o],[0,1-o],D),G=t[3],H=t[2],Z=t[1];if(o)var
u=H,p=G;else
var
u=G,p=H;var
v=b(c[65],d[1],u),_=bT(k,d[1],n,p,v);if(b(O[2][3],v,_))throw[0,ab,a(f[3],vp)];var
I=fR(k,d[1],n,v,0,p),h=I[1],w=h[1],$=I[2],y=Q(d[1],h,u),e=b(c[65],d[1],y),j=Q(d[1],h,p),l=Q(d[1],h,Z),J=Q(d[1],h,D),K=em(e-1|0,w),z=K[1],L=a(C[1][1][1],K[2]),m=g(c[i][13],d[1],1,Y),aa=0===o?0===m?u2:u1:0===m?u0:uZ,M=aa(d),ac=Q(d[1],h,q),N=b(c[70],d[1],ac)[3];if(m)var
P=a(r[47],N),ad=b(c[37],P,z),ae=[0,L,b(c[i][1],-e|0,l),ad],R=a(c[19],ae),A=P;else
var
aq=g(c[i][2],1,e+1|0,N),ar=a(c[9],e),W=b(c[i][5],ar,aq),as=a6(1,z),at=b(c[37],W,as),au=[0,E,b(c[i][1],1-e|0,J),at],av=a(c[19],au),aw=[0,L,b(c[i][1],-e|0,l),av],R=a(c[19],aw),A=W;var
S=b(c[i][1],e,R),af=V(1,z),T=d9(e,j,w),ag=b(F[x],e-1|0,T)[1];if(m)var
ah=[0,b(c[i][1],-e|0,j),0],U=g(c[i][3],ah,e-1|0,A);else
var
am=[0,js(d),[0,l,j]],an=a(c[21],am),ao=[0,b(c[i][1],-e|0,j),0],ap=[0,b(c[i][1],-e|0,an),ao],U=g(c[i][3],ap,e-1|0,A);var
ai=ce(d[1],j),aj=cJ(0,k,d[1],e,ai,w),ak=al(0,k,[0,d[1]],aj,h);return[0,dz(k,d,[0,n,q],[0,T,U],function(g){var
h=b(c[38],g,ag),f=b(c[i][1],e,h),k=m?a(c[21],[0,M,[0,l,S,j,f,y]]):a(c[21],[0,M,[0,l,j,S,f,y]]),n=b(c[i][1],1,k),o=[0,n,[0,a(c[9],1)]],p=[0,a(c[21],o),af],q=[0,E,J,a(c[21],p)],r=a(c[19],q);return Q(d[1],$,r)}),ak]}function
vq(e,d,q){var
k=q[2],h=q[1],m=bn(d[1],k),s=m[2],K=m[3],L=m[1],n=bZ(e,d[1],h,0,0,0,s),w=n[3],y=n[2],M=n[1],N=b(c[82],d[1],y)[1],O=b(c[82],d[1],w)[1],z=b(c[56],d[1],N),P=z?b(c[56],d[1],O):z;if(1-P)throw[0,ab,a(f[3],vr)];try{var
Q=g(_[71],e,d[1],M)}catch(b){b=G(b);if(b===X)throw[0,ab,a(f[3],vs)];throw b}var
A=a(_[12],Q),C=A[2],R=A[1];if(a(F[55],C))return cR(e,d,[0,h,k]);var
D=dl(R),E=D[2],j=du(e,d[1],D[1]),S=j[8],T=j[5],V=j[2];d[1]=j[1];var
Y=a(p[9],E),o=b(c[i][4],Y,S),Z=[0,jt(d),[0,o]],$=a(c[21],Z),I=b(c[x],h,e);try{var
af=b(ad[30],0,I),ag=g(B[65],af,d,$)}catch(c){c=G(c);if(c===X){var
ac=g(r[W],I,d[1],o),ae=a(f[3],vt);throw[0,ab,b(f[12],ae,ac)]}throw c}var
ah=f8(d[1],T),ai=a(H[7],ah)[3],aj=a(r[47],ai),ak=a(F[9],C),J=u(eB),al=b(c[i][4],ak,aj),am=v===J?eB[1]:l===J?a(t[2],eB):eB,an=b(U[55],d[1],[0,V,E]),ao=b(c[x],h,e),ap=g(a_[9],ao,d[1],an),aq=[0,[0,o],[0,[0,ag],[0,[0,ap],[0,[0,al],[0,[0,y],[0,[0,w],[0,[0,a(c[19],[0,L,s,K])],vu]]]]]]],ar=aa(h);return[0,ch(e,d,[0,h,k],h,[1,am],aq),ar]}function
vv(h,d,m){var
n=m[2],e=m[1],i=bn(d[1],n),o=i[2],s=i[3],w=i[1],j=bZ(h,d[1],e,0,0,0,o),k=j[1],y=j[3],z=j[2],A=[0,uY(d),[0,k]],C=a(c[21],A),p=b(c[x],e,h);try{var
F=b(ad[30],0,p),H=g(B[65],F,d,C)}catch(c){c=G(c);if(c===X){var
D=g(r[W],p,d[1],k),E=a(f[3],vw);throw[0,ab,b(f[12],E,D)]}throw c}var
q=u(eA),I=v===q?eA[1]:l===q?a(t[2],eA):eA,J=[0,[0,k],[0,[0,H],[0,[0,z],[0,[0,y],[0,[0,a(c[19],[0,w,o,s])],vx]]]]],K=aa(e);return[0,ch(h,d,[0,e,n],e,[1,I],J),K]}function
jz(i,d,n){var
j=n[2],h=n[1];try{var
y=function(a){var
e=b(c[x],h,i);return g(a_[11],e,d[1],a)},o=function(h){var
b=cD(d[1],h),c=b[2],e=b[1],g=u(bY),i=v===g?bY[1]:l===g?a(t[2],bY):bY,j=1-a(jw(d[1],i),e),k=j||1-(8===c.length-1?1:0);if(k)throw[0,ab,a(f[3],vy)];return[0,e,c]};try{var
M=o(j)[2],e=M,q=j}catch(a){a=G(a);if(a[1]!==ab)throw a;var
p=y(j),e=o(p)[2],q=p}var
k=T(e,0)[1],z=T(e,1)[2],m=T(e,2)[3],r=T(e,3)[4],s=T(e,4)[5],A=T(e,6)[7],B=T(e,7)[8],C=[0,uV(d),[0,k,m]],D=a(c[21],C),E=[0,uW(d),[0,k,m,r,s]],F=[0,D,a(c[21],E)],H=[0,js(d),F],I=a(c[21],H);if(1-f7(i,d[1],h,B,I))throw[0,ab,a(f[3],vz)];var
w=u(eC),J=v===w?eC[1]:l===w?a(t[2],eC):eC,K=aa(h),L=[0,ch(i,d,[0,h,q],h,[1,J],[0,[0,k],[0,[0,z],[0,[0,m],[0,[0,r],[0,[0,s],[0,[0,A],vA]]]]]]),K];return L}catch(a){a=G(a);if(a[1]===ab)return cR(i,d,[0,h,j]);throw a}}function
jA(a,b,c){return cQ(vv,vq,a,b,c)}function
jB(d,c,b){return a(E[3],vB)}function
jC(o,d,n){var
h=n[2],e=n[1],j=bn(d[1],h),k=j[3],m=j[2],p=j[1],q=u(bX),x=v===q?bX[1]:l===q?a(t[2],bX):bX;if(1-a(cS(d[1],x),m))throw[0,ab,a(f[3],vC)];var
s=aa(e);if(g(c[i][13],d[1],1,k)){var
y=function(d){var
e=[0,p,m,b(c[i][1],1,d)];return a(c[19],e)};return[0,dz(o,d,[0,e,h],[0,e,a(r[47],k)],y),s]}var
w=u(ez),z=a(c[19],[0,p,m,k]),A=v===w?ez[1]:l===w?a(t[2],ez):ez;return[0,ch(o,d,[0,e,h],e,[1,A],[0,[0,z],vD]),s]}function
jD(w,d,j){var
k=j[1],e=bn(d[1],j[2]),h=e[3],m=e[2],n=u(bW),y=e[1],z=v===n?bW[1]:l===n?a(t[2],bW):bW;if(1-a(cS(d[1],z),m))throw[0,ab,a(f[3],vE)];var
A=aa(k);if(g(c[i][13],d[1],1,h))var
B=a(r[47],h),q=B,p=cg(uu,d);else
var
D=a(c[19],[0,y,m,h]),q=D,p=cg(uw,d);var
s=a(c[21],[0,p,[0,q]]),C=b(c[x],k,w);o(aq[3],0,C,d,s);return[0,[0,0,s],A]}function
f9(B,A,i,d,o){var
p=o[2],j=o[1],q=u(bY),C=cD(d[1],p)[1],D=v===q?bY[1]:l===q?a(t[2],bY):bY;if(a(jw(d[1],D),C))return 0;var
k=bn(d[1],p)[2],s=u(bW),E=v===s?bW[1]:l===s?a(t[2],bW):bW;if(a(cS(d[1],E),k))return 3;var
w=u(bX),H=v===w?bX[1]:l===w?a(t[2],bX):bX;if(a(cS(d[1],H),k))return 2;var
m=bZ(i,d[1],j,0,0,0,k),e=m[3],h=m[2],I=m[1];if(A){if(b(c[44],d[1],h))if(b(c[44],d[1],e)){b(c[65],d[1],e);b(c[65],d[1],h);return[1,0]}if(b(c[44],d[1],h))return vF;if(b(c[44],d[1],e))return vG;throw[0,ab,a(f[3],vH)]}function
n(f,e){var
a=b(c[65],d[1],f),g=bT(i,d[1],j,e,a);return 1-b(O[2][3],a,g)}if(b(c[44],d[1],h))if(b(c[44],d[1],e))if(n(h,e)){b(c[65],d[1],e);b(c[65],d[1],h);return[1,0]}if(b(c[44],d[1],h))if(n(h,e))return vI;if(b(c[44],d[1],e))if(n(e,h))return vJ;function
J(a){var
e=b(c[82],d[1],a)[1];try{var
f=b(c[5],d[1],e);b(cO[1],i,f);var
g=1;return g}catch(a){a=G(a);if(a===X)return 0;throw a}}function
y(a){var
e=b(c[82],d[1],a)[1],f=b(c[x],j,i),h=g(a_[11],f,d[1],e);return b(c[56],d[1],h)}if(f7(i,d[1],j,h,e))return vK;if(J(I))if(y(h))if(y(e))return[2,[0,[0,B,2],0]];function
z(e,i){function
a(e){if(g(c[94],d[1],e,i))throw r[28];var
f=b(c[82],d[1],e),j=f[2],h=b(c[56],d[1],f[1]);return h?b(F[14],a,j):h}try{a(e);var
f=0;return f}catch(a){a=G(a);if(a===r[28])return 1;throw a}}if(!z(h,e))if(!z(e,h))throw[0,ab,a(f[3],vL)];return 1}function
vM(f,e,d,h){var
i=h[1],j=g(U[27],e,d[1],h[2]),k=bn(d[1],j)[2];try{var
m=g(U[27],e,d[1],k),n=bZ(e,d[1],i,0,0,0,m)[1],o=function(k,j){var
g=k,e=j;for(;;){var
m=b(U[26],d[1],g),h=cD(d[1],m),i=u(dy),n=h[2],o=h[1],p=v===i?dy[1]:l===i?a(t[2],dy):dy;if(a(cS(d[1],p),o)){var
q=[0,a(c[9],1),0],r=[0,T(n,1)[2],q],g=b(U[55],d[1],r),e=[0,f,e];continue}return e}}(n,[0,f,0]);return o}catch(a){a=G(a);if(a[1]===ab)return[0,f,0];throw a}}function
jE(a){if(typeof
a==="number")switch(a){case
0:return jz;case
1:return jB;case
2:return jC;default:return jD}else
switch(a[0]){case
0:var
b=a[1];return function(a,c,d){return jx(b,a,c,d)};case
1:var
c=a[1];return function(a,b,d){return jy(c,a,b,d)};default:var
d=dA(a[1]),e=function(a,b,c){return cQ(d,jA,a,b,c)};return function(a,b,c){return cQ(jz,e,a,b,c)}}}function
vN(d){var
a=d[2],b=d[1];function
e(e,d,c,a){try{var
f=g(e,d,c,a);return f}catch(a){a=G(a);if(a[1]===ab)return bR([0,b,vO,a[2]]);throw a}}function
c(d){function
a(c,b,a){return g(jE(g(d,c,b,a)),c,b,a)}function
b(b,c,d){return cQ(a,vk,b,c,d)}function
c(a,c,d){return ve(b,a,c,d)}return function(a,b,d){return e(c,a,b,d)}}if(typeof
a==="number")switch(a){case
0:var
f=0;return c(function(a,c,d){return f9(b,f,a,c,d)});case
1:var
h=1;return c(function(a,c,d){return f9(b,h,a,c,d)});default:return function(d,c,a){var
f=[0,b,0];return g(dA(e(function(a,b,c){return vM(f,a,b,c)},d,c,a)),d,c,a)}}var
i=a[1];return c(function(c,b,a){return i})}function
dA(c){var
a=b(p[19],vN,c);return a?g(p[20],cQ,a[1],a[2]):cR}function
f_(j){function
d(l){var
d=a(k[66][1],l),m=a(k[66][5],d),n=a(aA[41],m),o=a(k[66][4],d);function
q(b){var
c=a(C[2][1][1],b);return a(r[e6],c)}var
e=b(F[aY],q,o),s=e[1],t=b(c[aY],e[2],n),f=cB(s),h=f[1],u=f[2],v=cA(vQ,function(a){throw[0,P,vP]},h)[2],w=a(k[66][3],d),x=b(c[i][11],u,w);function
y(e){var
d=[0,e],f=g(dA(j),t,d,[0,h,x])[1][2],k=a(p[9],v),l=b(c[i][4],k,f);return[0,d[1],l]}return b(cf[2],1,y)}return a(k[66][10],d)}function
v0(d){var
c=d[2];if(typeof
c==="number")switch(c){case
0:return a(f[3],v1);case
1:return a(f[3],v2);default:return a(f[3],v3)}var
b=c[1];if(typeof
b==="number")switch(b){case
0:return a(f[3],vR);case
1:return a(f[3],vS);case
2:return a(f[3],vT);default:return a(f[3],vU)}else
switch(b[0]){case
0:return 0===b[1]?a(f[3],vV):a(f[3],vW);case
1:return 0===b[1]?a(f[3],vX):a(f[3],vY);default:return a(f[3],vZ)}}var
dB=b(f[39],f[13],v0);aH(1086,[0,ab,ju,u9,cQ,jx,jy,jA,jB,jC,jD,cR,jE,f9,dA,f_,dB],"Simplify");function
dC(c,d){function
g(d){switch(d[0]){case
0:var
h=d[4],j=d[3],k=d[2],l=d[1];if(0===h[0]){var
n=h[1],o=function(d){var
e=a(c,d[8]),f=a(c,d[7]),h=a(c,d[6]),i=a9(c,d[5]),j=b(h8,c,d[4]),k=d[3],l=d[2],m=d[1];return[0,m,l,k,j,i,h,f,e,g(d[9])]},p=b(e[17][15],o,k),q=a9(c,l),r=[0,a(c,n)];return[0,q,p,a(c,j),r]}var
s=a9(c,l);return[0,s,k,a(c,j),h];case
1:var
t=d[4],u=d[3],v=d[2],w=a9(c,d[1]),x=a(H[16],g),y=b(e[19][15],x,t);return[1,w,v,a(c,u),y];case
2:var
z=d[6],A=d[5],B=d[4],C=d[3],D=d[2],E=a9(c,d[1]),F=function(a){var
d=a[4],f=a[3],h=a[2],i=a[1],j=g(a[5]),k=a9(c,f);return[0,i,b(e[17][15],c,h),k,d,j]},G=b(e[17][15],F,z);return[2,E,a(c,D),C,B,A,G];case
3:var
I=d[2],J=a9(c,d[1]);return[3,J,g(I)];case
4:var
K=d[1];return[4,K,g(d[2])];default:var
f=d[2],L=d[3],M=a9(c,d[1]),i=f[1],m=f[6],N=i[3],O=i[2],P=i[1],Q=m[2],R=m[1],S=g(L),T=a(c,f[10]),U=a9(c,f[9]),V=a9(c,f[8]),W=a9(c,f[7]),X=b(e[17][15],c,Q),Y=[0,a(c,R),X],Z=f[5],_=f[4],$=f[3],aa=a(c,f[2]),ab=a(c,N);return[5,M,[0,[0,P,a(c,O),ab],aa,$,_,Z,Y,W,V,U,T],S]}}return g(d)}function
f$(f,d,j,i,h){var
b=g(B[62],j,f,i),k=b[3],l=ia(b[1],d,b[2],[0,h],f),m=[0,d,a(e[19][12],k)];return[0,l,a(c[12],m)]}function
ga(aR,q,X,h){var
n=[0,aJ[8][1]],F=[0,0];function
s(d,p,aS){var
h=aS;for(;;)switch(h[0]){case
0:var
K=h[4],Y=h[3],Z=h[2],L=h[1][1];if(0===K[0]){var
aT=K[1],aU=[0,p,L],aW=function(d,l){var
f=d[4],m=d[1],x=l[2],y=l[1],z=d[9],A=d[7],o=b(c[aY],f,X),h=s(o,y,z),k=h[3],p=h[1],B=h[2],r=b(e[17][15],aZ,f),C=b(c[i][11],r,k),t=b(c[3],p,A);if(3===t[0]){var
D=t[1][1],E=b(c[i][1],1,k),G=[0,[0,a(j[1][6],v5)],B,k,E],u=f$(p,D,o,a(c[20],G),[0,av,[3,v6,[0,m]]]),v=u[2],H=u[1],w=b(c[75],q[1],v)[1],I=n[1],J=a(e[17][1],f);n[1]=g(aJ[8][4],w,J,I);F[1]=[0,[0,w,0],F[1]];return[0,H,[0,ay([0,m],[0,b(c[i][11],r,v)],C),x]]}throw[0,P,v4]},$=g(e[17][19],aW,Z,aU),aa=$[2],ab=$[1],aX=b(c[38],aT,aa);return[0,ab,aX,b8(d,ab,Y,aa)]}var
a0=K[1];if(a(e[17][55],Z)){var
ac=u(bx),a1=v===ac?bx[1]:l===ac?a(t[2],bx):bx,ad=a5(p,a1),a2=ad[2],a3=ad[1],a4=b6((a(e[17][1],L)-a0|0)+1|0),a7=[0,a(c[8],a4)],a8=ay([0,a(j[1][6],v7)],a7,a2),ae=b(c[37],Y,L),a9=b(c[i][1],1,ae),af=cC(d,a3,[0,[0,av,v8]],b(c[36],a8,a9)),ag=af[2],ah=af[1],a$=b(c[75],ah,ag)[1];n[1]=g(aJ[8][4],a$,0,n[1]);return[0,ah,ag,ae]}throw[0,P,v9];case
1:var
ai=h[4],I=h[3],D=h[2],aj=h[1],y=aj[1];if(cp[1]){var
m=[0,p],z=jf(d,m,y,D,I),ak=z[7],am=z[5],an=z[4],M=z[1],ba=z[6],bc=z[3],bd=z[2],be=ak?b(dA(v_),d,m):function(a){return cR(d,m,a)},bg=function(n,l){var
T=n[3],v=g(c[91],q[1],n[2],n[1]),y=v[2],j=g(aV[19],d,q[1],v[1]);if(ak)var
u=0;else
if(0<an)var
u=0;else
var
z=y,u=1;if(!u)var
U=b(e[18],j,M),X=b(c[x],U,d),z=g(a_[11],X,m[1],y);var
A=g(c[91],q[1],an,z),B=A[2],p=A[1];if(bf[1]){var
Y=b(e[18],j,M),D=b(e[18],p,Y),Z=a(f[3],v$);b(at[6],0,Z);var
_=b(c[x],D,d),$=g(r[W],_,m[1],B);b(at[6],0,$);var
aa=a(f[3],wa);b(at[6],0,aa);var
ab=cG(d,m[1],D);b(at[6],0,ab)}var
ac=b(e[18],j,M),F=a(be,[0,b(e[18],p,ac),B]),G=F[1],H=G[2],I=G[1],ad=F[2],ae=al(wb,d,[0,m[1]],T,aj),af=al(wc,d,[0,m[1]],ad,ae);if(I)if(l){var
J=l[1],K=I[1],L=K[2],N=K[1][1],O=s(d,m[1],J),h=O[1],ag=O[2],ah=V(0,N),ai=id(q[1],ag,ah),am=Q(h,iE([0,d],h,af,iF(J)),ai),P=b(w[24],h,L[1]),ao=a(w[6],P),ap=function(b){var
d=a(C[2][1][1],b);return a(c[10],d)},ar=b(e[17][15],ap,ao),as=a(e[17][1],N),R=b(e[17][x],as,ar),au=R[2],av=R[1],aw=a(aA[9],d),ax=function(c,b){return[0,a(C[2][1][1],c),b]},ay=g(e[17][21],ax,aw,au),az=b(c[i][9],ay,am),S=b(c[i][4],av,az),aB=a(w[12],P);o(aq[2],0,aB,h,S);var
aC=b(c[5],h,S);m[1]=g(w[31],L[1],aC,h);var
t=H,k=1}else
var
k=0;else
if(l)var
k=0;else
var
t=H,k=1;if(!k)var
t=a(E[3],wd);var
aD=b(e[18],p,j);return b(c[38],t,aD)},bh=g(e[19][57],bg,bc,ai),bi=Q(m[1],am,bd),bj=m[1],bk=function(a){return Q(bj,am,a)},bl=b(e[19][15],bk,bh),bm=em(D-1|0,y)[2],bn=a(C[1][1][3],bm),bo=b(c[i][1],D,bn),bp=a(c[9],D),ao=g(_[72],d,m[1],bo),ap=ao[1],bq=ao[2],br=g(_[76],d,ap[1],4),bs=[0,bQ(m[1],ap),bq],bt=a(_[5],bs),bu=aF(_[77],d,m[1],bt,br,bi,bp,bl),bv=[0,bu,a(e[19][12],ba)],bw=a(c[21],bv),by=b(c[38],bw,y),as=b8(d,p,I,y),au=b(B[35],m[1],by);o(aq[5],d,m,au,as);return[0,m[1],au,as]}var
J=[0,p],aw=u(bx),bz=fO(D-1|0,y)[1],bA=v===aw?bx[1]:l===aw?a(t[2],bx):bx,N=ar(J,bA),bB=function(e){if(e){var
b=s(d,J[1],e[1]),f=b[3],g=b[2];J[1]=b[1];return[0,g,f]}var
h=b6(D);return[0,a(c[8],h),N]},ax=b(e[19][15],bB,ai),bC=J[1],bD=function(d,c){var
e=c[2],f=[0,c[1]],g=a(E[22],d),h=b(E[17],we,g);return ay([0,a(j[1][6],h)],f,e)},bE=b(e[19][16],bD,ax),bF=function(d,f){var
e=d[1],g=d[2];return[0,e+1|0,[0,b(bb,a(c[i][1],e),f),g]]},az=g(e[19][17],bF,wf,bE)[2],bG=a6(ax.length-1,y),bH=b(c[37],I,bG),bI=b(c[38],bH,az),bJ=b6(a(e[17][1],az)),bK=[0,a(c[8],bJ)],bL=ay([0,a(j[1][6],wg)],bK,N),bM=b6(a(e[17][1],bz)),bN=[0,a(c[8],bM)],bO=[0,bL,[0,ay([0,a(j[1][6],wh)],bN,N),0]],bP=b(c[i][1],2,bI),aB=cC(d,bC,[0,[0,av,[3,aR,0]]],b(c[38],bP,bO)),aC=aB[2],O=aB[1],bR=b(c[75],O,aC)[1];n[1]=g(aJ[8][4],bR,0,n[1]);var
aD=b8(d,O,I,y);return[0,O,a(c[17],[0,aC,2,aD]),aD];case
2:var
aG=h[5],aH=h[1][1],bS=h[6],bT=aG[2],bU=aG[1],bV=h[3],bW=h[2],bX=function(a){var
c=a[5],e=a[2];function
f(b){var
a=s(d,b,c),f=a[1];return[0,f,aE(a[2],e)]}return b(cf[2],0,f)},bY=b(e[17][15],bX,bS),bZ=a(k[37],bY),b0=a(k[64][1],p),b1=b(k[18],b0,bZ),aI=g(k[15],d,b1,bT)[2],b2=b(k[7],bU,aI),b3=a(e[17][5],b2),b4=b(c[37],bW,aH),b5=b(c[i][11],bV,b3),b7=b(c[38],b5,aH);return[0,a(k[6],aI),b7,b4];case
3:var
aK=h[1],aL=aK[1],b9=aK[2],R=s(d,p,h[2]),G=R[1],b_=R[3],b$=R[2],ca=it(wi,d,G,b9)[2],aM=a(e[19][70],ca),cb=a(c[21],[0,b$,aM]),cc=b(U[24],G,cb),cd=b(c[38],cc,aL);return[0,G,cd,b8(d,G,fD(G,b_,aM),aL)];case
4:var
h=h[2];continue;default:var
H=h[2],aN=h[1][1],aO=H[6],S=H[5],ce=aO[2],cg=aO[1],ch=H[3],ci=H[2],cj=H[1][1],T=s(d,p,h[3]),aP=T[3],ck=T[2],cl=T[1],cm=b(c[i][1],1,aP),cn=[0,[0,a(j[1][6],wj)],ck,aP,cm],co=a(c[20],cn),aQ=f$(cl,S,a(A[2],0),co,[0,av,[3,wk,[0,cj]]])[1];n[1]=g(aJ[8][4],S,0,n[1]);F[1]=[0,[0,S,ch],F[1]];var
cq=a(c[34],[0,cg,ce]),cr=b(c[38],cq,aN);return[0,aQ,cr,b8(d,aQ,ci,aN)]}}var
d=s(X,q[1],h),m=d[3],p=d[2];q[1]=d[1];return[0,F[1],n[1],p,m]}function
jF(g,d,b){if(d){var
e=d[1];if(typeof
b!=="number"&&0===b[0]){var
f=b[1];if(1===f[0]){var
h=b[2][1],i=f[1];return 0===e[0]?eh(g,e,a(c[22],i)):0===h?1:0}}return 0}return 0}var
wl=b(be[8][11],be[8][10],be[8][7]),wm=[0,a(a_[15],wl),2],wn=a(s[50],wm);function
cT(b){return a(e[8],b[3])}function
gb(L,_,J,q,I,d,G,p,F,E,Z){var
f=p[1],$=p[3],aa=p[2];d[1]=a(B[45],d[1]);var
i=ga(I,d,G,E),M=i[2],ab=i[4],ac=i[3],ad=i[1];a(B[48],d);var
ae=dC(a(B[35],d[1]),E),af=b(U[29],d[1],ab),ag=b(c[5],d[1],af),ah=b(c[5],d[1],ac),r=aF(aT[5],G,f,d[1],0,[0,I],ah,ag),t=r[3],N=r[2],O=N[1],k=[0,j[1][10][1]],u=[0,j[1][10][1]],ai=N[2],aj=r[1];function
ak(c){var
h=c[3],e=c[1],o=c[5],p=c[4],q=c[2],i=a(hb(j[1][1],e),O);if(b(aJ[8][3],i,M))var
r=b(aJ[8][22],i,M),t=hI(0),v=b(m[66][29],r,s[16]),f=[0,b(m[66][3],v,t)];else
if(jF(d[1],F,h[2])){k[1]=b(j[1][10][4],e,k[1]);var
l=a(H[7],F);if(0===l[0])var
n=m[1];else
var
C=l[1][1],D=m[1],E=function(b){return z(a(s[68],[0,[0,wo,[1,b]],0]))},n=g(H[24],E,D,C);var
w=aT[6][1],x=[0,hJ(0),0],y=[0,aB(n),x],A=a(m[66][20],[0,wn,[0,s[28],y]]),B=a(m[66][22],A),f=[0,b(m[66][12],B,w)]}else{u[1]=b(j[1][10][4],e,u[1]);var
f=[0,aT[6][1]]}return[0,e,q,h,p,o,f]}var
l=b(e[19][15],ak,aj);function
al(a){var
c=a[1],d=a[2];return[0,c,d,b(e[17][36],c,O)]}var
am=b(e[17][15],al,ad);function
an(d,c){function
h(a){return b(S[1],0,[1,a[1]])}var
i=b(e[19][52],h,l);b(cN[2][88],1,i);var
m=a(j[1][8],f),n=b(j[1][10][7],k[1],u[1]);return g(Z,ae,ai,[0,c,m,[0,d,J,0],am,k[1],n])}var
v=a(bV[1],an);function
x(e){var
f=be[14],g=a(c[8],e),h=d[1],i=a(A[2],0),j=o(U[15],f,i,h,g);return b(c[5],d[1],j)}var
y=[0,2,J,0],n=b(c[37],$,aa);if(L){var
P=L[1];if(0===P[0]){var
C=P[1];if(C)if(!C[2]){var
as=C[1],at=b(c[37],n,[0,[0,0,n],0]),au=b(c[5],d[1],at),D=as[2];if(0===D[0]){var
Q=D[1][2];if(Q)var
R=Q[1],V=R[2],T=R[1],h=1;else
var
h=0}else{var
W=D[1];if(W){var
X=W[1][2];if(X)var
Y=X[1],V=Y[2],T=Y[1],h=1;else
var
h=0}else
var
h=0}var
av=h?[0,b(S[1],[0,T],V)]:0,aw=a(w[br],d[1]);Oz(aT[8],[0,[0,f,t,au,q,l],0],aw,0,0,[0,y],[0,x],[0,v],0,0,[0,[0,[0,av,0],0]]);return 0}var
ao=b(c[37],n,_),ap=b(c[5],d[1],ao),aq=a(w[br],d[1]),ar=b(K[5],f,wp);bL(aT[7],ar,[0,t],ap,aq,0,[0,q],[0,y],0,[0,x],[0,v],0,l);return 0}}var
ax=b(c[5],d[1],n),ay=a(w[br],d[1]);bL(aT[7],f,[0,t],ax,ay,0,[0,q],[0,y],0,[0,x],[0,v],0,l);return 0}function
jG(g,d,c){if(0===c[0])return[0,Q(g,d,c[1])];var
h=c[1];try{var
i=a(e[8],d),f=b(e[17][7],i,h-1|0);if(0===f[0]){var
j=[1,f[1]];return j}throw[0,P,wr]}catch(a){a=G(a);if(a===X)throw[0,P,wq];throw a}}function
gc(d,c,b){return 0===b[0]?[0,a(d,b[1])]:[1,a(c,b[1])]}function
jH(d,f,e){var
g=b(c[5],d,e);return b(f,function(e){var
f=[0,a(aN[34],e)],g=b(S[1],0,f),h=a(ao[18],g),i=a(A[2],0),c=b(A[47],i,h),j=c[1],k=dm(c[2])[1],l=[0,a(au[16],j),k],m=a(aO[35],l);return b(B[46],d,m)},g)}function
gd(b,a){function
c(c){return jH(b,a,c)}return function(a){return dC(c,a)}}aH(1087,[0,f$,ga,jF,cT,gb,jG,gc,jH,dC,gd],gV);function
jI(k,i){var
c=k,a=i;for(;;){if(c){var
e=c[1],l=c[2];if(a){var
d=a[1],m=a[2];if(0===e[0])var
g=e[1],f=0===d[0]?b(aJ[4],g,d[1]):-1;else
var
h=e[1],f=0===d[0]?1:b(j[1][2],h,d[1]);if(0===f){var
c=l,a=m;continue}return f}return-1}return a?1:0}}var
ag=a(e[21][1],[0,jI]),jJ=ag[22],ws=ag[1],wt=ag[2],wu=ag[3],wv=ag[4],ww=ag[5],wx=ag[6],wy=ag[7],wz=ag[8],wA=ag[9],wB=ag[10],wC=ag[11],wD=ag[12],wE=ag[13],wF=ag[14],wG=ag[15],wH=ag[16],wI=ag[17],wJ=ag[18],wK=ag[19],wL=ag[20],wM=ag[21],wN=ag[23],wO=ag[24];function
jK(d,c){try{var
g=d[4],h=function(d){var
e=d[3],f=a(au[16],c),g=a(aN[34],e),h=a(ao[9],g);return b(au[5],h,f)},i=b(e[17][31],h,g);return i}catch(b){b=G(b);if(b===X)return fA(0,a(f[3],wP));throw b}}function
jL(c){var
b=a(aI[15],wQ);return a(aI[14][14],b)}function
wR(b){return z(a(s[62],[0,b,0]))}var
wS=a(m[53],wR),wT=z(s[61]),wU=b(m[5],wT,wS);function
dD(c,a){var
d=b(e[18],a,wV),f=[0,jL(0)];return z(L(ge[7],0,f,0,c,d))}function
gf(a){return b(E[17],a[2],wW)}function
wX(b){var
c=dD(0,b),d=[0,a(m[21],c),0],e=g(ci[2],0,m[66][2],b),f=[0,a(k[70][8],e),d],h=a(m[7],f);return a(m[17],h)}function
gg(c,b){var
d=dD(0,b),e=[0,a(m[21],d),0],f=g(ci[6],0,b,c),h=[0,a(k[70][8],f),e],i=a(m[7],h);return a(m[17],i)}function
wY(b){var
c=g(ci[2],0,m[66][2],[0,b,0]),d=a(k[70][8],c);return a(m[17],d)}function
cU(c){var
e=a(ci[4],c);function
d(c){if(c){var
e=c[1],l=c[2],h=a(au[16],e[1]),n=e[5]?cV[3]:cV[4],o=function(a){return b(n,0,a)},p=a(m[66][59],h),i=b(k[17],p,o),q=function(e){if(bf[1]){var
c=a(f[3],wZ);b(at[10],0,c)}return d(l)};if(bf[1])var
s=function(c){var
d=a(k[66][3],c),e=a(k[66][6],c),j=a(k[66][5],c),l=g(r[W],j,e,d),m=a(f[3],w0),n=a(bj[58],h),o=a(f[3],w1),p=b(f[12],o,n),q=b(f[12],p,m),s=b(f[12],q,l);b(at[10],0,s);return i},j=a(k[66][9],s);else
var
j=i;return b(k[22],j,q)}var
t=a(f[3],w2);return b(m[66][4],0,t)}var
h=d(e);return a(k[70][8],h)}function
jM(c,b,a){if(a){var
f=a[2],d=ck(B[4],c,b,0,0,0,0,0,0,a[1]),g=d[2],e=jM(c,d[1],f);return[0,e[1],[0,g,e[2]]]}return[0,b,0]}function
jN(o,h,n,m){var
e=o,j=n,i=m;for(;;){var
p=b(r[60],h,i),d=b(c[3],h,p);switch(d[0]){case
6:var
k=d[2],u=d[3],v=d[1];if(1===j)try{var
l=g(_[72],e,h,k)[1],w=[0,l[1][1],l[2]];return w}catch(a){a=G(a);if(a===X)return a7(w4);throw a}var
e=b(c[bu],[0,v,k],e),j=j-1|0,i=u;continue;case
8:var
x=d[4],e=b(c[bu],[1,d[1],d[2],d[3]],e),i=x;continue;default:var
q=g(r[W],e,h,i),s=a(f[3],w3),t=b(f[12],s,q);return g(af[6],0,0,t)}}}function
gh(u,p){function
d(D){function
d(d){function
h(av){var
q=b(e[17][15],k[10],av);function
H(e){var
f=b(w[23],d,e),g=a(w[5],f);return a(c[8],g)}var
l=b(e[17][15],H,q);function
I(c){var
e=b(w[23],d,c);return a(w[6],e)}var
J=b(e[17][15],I,q),v=a(e[17][gZ],J),x=v[1],K=v[2];function
L(a){return g(C[2][6],R[74],x,a)}var
m=b(e[17][25],L,K)?b(aA[32],x,D):D;if(u)var
h=b(e[17][15],j[1][6],u);else
var
au=function(f,e){var
c=b(w[50],e,d);if(c)return c[1];var
g=a(E[22],f),h=b(E[17],xg,g);return a(j[1][6],h)},h=b(e[17][16],au,q);var
n=a(e[17][1],h),r=a(e[17][1],p),s=a(e[17][1],l),y=n===r?1:0,M=y?n===s?1:0:y;if(1-M){var
N=b(e[15][46],s,w5),O=a(f[3],N),Q=a(f[16],s),S=a(f[3],w6),T=1===r?w7:xf,U=a(f[3],T),V=a(f[16],r),W=a(f[3],w8),Y=b(e[15][46],n,w9),Z=a(f[3],Y),_=a(f[16],n),$=a(f[3],w_),aa=b(f[12],$,_),ab=b(f[12],aa,Z),ac=b(f[12],ab,W),ad=b(f[12],ac,V),ae=b(f[12],ad,U),ag=b(f[12],ae,S),ah=b(f[12],ag,Q),ai=b(f[12],ah,O);g(af[6],0,w$,ai)}function
aj(c,b,a){return[0,c,b,a]}var
z=o(F[79],aj,h,p,l),A=a(e[17][5],z),ak=jN(m,d,A[2],A[3])[1];function
al(p,o){var
h=p,e=o;for(;;){if(e){var
i=e[1],l=i[3],k=i[1],q=e[2],r=jN(m,d,i[2],l)[1];if(1-b(j[23][13],ak,r))a7(xa);try{b(C[2][5],k,h);var
x=1,n=x}catch(a){a=G(a);if(a!==X)throw a;var
n=0}if(n){var
s=a(f[3],xb),t=a(bS[9],k),u=a(f[3],xc),v=b(f[12],u,t),w=b(f[12],v,s);g(af[6],0,xd,w)}var
h=[0,[0,k,b(c[5],d,l)],h],e=q;continue}return h}}var
am=al(a(aA[9],m),z);function
an(a){return a-1|0}var
B=b(e[19][53],an,p);function
ao(a){return[0,a]}var
ap=b(e[19][53],ao,h),t=[0,function(a){throw[0,P,xe]}];function
aq(f){var
g=a(aA[27],am),d=jM(b(aA[42],g,m),f,l),j=d[2],k=d[1],n=a(e[17][9],h),o=a(c[i][11],n),p=b(e[19][53],o,j),q=[0,ap,a(e[19][12],l),p];t[1]=function(b){return a(c[31],[0,[0,B,b],q])};return[0,k,a(t[1],0)]}var
ar=b(cf[2],0,aq);function
as(c){function
d(b){return[0,b,a(t[1],c+1|0)]}return b(cf[2],0,d)}var
at=[0,ar,b(e[17][56],B.length-1-1|0,as)];return a(k[37],at)}return b(k[71][1],k[64][6],h)}return b(k[71][1],k[54],d)}return b(k[71][1],k[55],d)}function
eJ(e,d,c){var
b=jK(e,d),a=b[2],f=b[1];return[0,f,a,T(c,a)[a+1]]}function
jO(d,i,h,g){function
f(g,a){var
i=0;function
j(g,a,j){if(g)return g;var
e=b(c[82],d,j);switch(a[0]){case
0:var
i=e[1];if(a[1]===h)return[0,b(c[67],d,i)];break;case
1:return f(a[2],e[2])}return 0}return o(e[17][23],j,i,g,a)}var
j=f(a(e[17][9],i),g);return a(H[7],j)}function
dE(d){var
e=a(y[7],d),f=a(y[2],d);switch(b(c[3],f,e)[0]){case
6:var
h=z(s[16]);return g(m[5],h,dE,d);case
8:var
i=z(s[58]);return g(m[5],i,dE,d);default:return a(m[1],d)}}function
eK(d){var
c=[0,z(b(s[x],0,0)),0];return a(m[7],c)}function
jP(a){return 1===a[0]?[0,a[4]]:0}function
jQ(a){return 5===a[0]?[0,a[3]]:0}function
jR(a){return 0===a[0]?[0,a[2]]:0}function
eL(c,b){return b?a(c,b[1]):0}function
gi(d){function
e(e){var
f=u(ap);function
g(g){var
f=u(ah);function
h(f){function
h(h){var
i=[0,0,g,f,b(y[42][8],h,e)],j=a(c[20],i),l=[0,gf(d),0],m=aB(dD(xi,[0,d[2],l])),n=L(s[bN],0,0,e,[0,j],xh);return b(k[18],n,m)}return a(k[66][10],h)}var
i=v===f?ah[1]:l===f?a(t[2],ah):ah,j=a(m[66][59],i);return b(k[17],j,h)}var
h=v===f?ap[1]:l===f?a(t[2],ap):ap,i=a(m[66][59],h);return b(k[17],i,g)}var
f=a(m[66][59],d[1]);return b(k[17],f,e)}function
jS(h,g,b,f){var
d=a(e[19][8],g);T(d,b)[b+1]=f;return a(c[21],[0,h,d])}function
b0(d,n,q,p,N){var
h=N;for(;;)switch(h[0]){case
0:var
B=h[2],O=h[4],F=eL(jR,q);if(F)var
Q=F[1],R=function(a){return[0,a]},I=b(e[17][15],R,Q);else
var
ai=function(a){return 0},I=b(e[17][15],ai,B);if(a(e[17][55],B))var
D=m[1];else{var
ab=function(J,h,k){if(k){var
l=k[1];try{var
u=a(e[17][5],l[2]);if(0!==u[0])throw[0,P,xy];var
v=b(aJ[8][22],u[1],d[3])}catch(a){a=G(a);if(a===X)throw[0,P,xp];throw a}var
K=v[1],L=l[4],M=[0,v[2],p],N=a(e[17][1],l[4]),D=K,B=n[1]+N|0,y=M,x=L}else
var
aT=h[4],aU=a(e[17][1],h[4]),D=h[7],B=n[1]+aU|0,y=p,x=aT;var
O=[0,B,n[2]],Q=0,R=h[9];function
S(a){return a[9]}var
T=[0,b0(d,O,b(H[16],S,k),y,R),Q],U=a(H[3],k)?m[1]:cU(b(E[17],d[1][2],xx)),V=[0,z(s[28]),[0,U,T]],Y=z(b(s[81],fq,1)),Z=[0,a(m[21],Y),V],_=aj(xq,a(m[7],Z));try{var
aS=b(jJ,h[2],d[2]),o=aS}catch(a){a=G(a);if(a!==X)throw a;var
o=a7(xr)}var
F=o[1],$=o[2];if(bf[1]){var
q=a(A[2],0),aa=az(q,w[16],h[5]),ab=a(f[3],xs),ac=function(b){var
d=a(c[8],b);return g(r[W],q,w[16],d)},ad=g(f[39],f[13],ac,$),ae=a(f[3],xt),af=g(r[W],q,w[16],h[7]),ag=a(f[3],xu),ah=a(bS[9],h[1]),ai=a(f[3],xv),ak=a(j[1][8],F),al=a(f[3],ak),am=a(f[3],xw),an=b(f[12],am,al),ap=b(f[12],an,ai),aq=b(f[12],ap,ah),ar=b(f[12],aq,ag),as=b(f[12],ar,af),au=b(f[12],as,ae),av=b(f[12],au,ad),aw=b(f[12],av,ab),ax=b(f[12],aw,aa);b(at[10],0,ax)}var
ay=a(aN[34],F),aA=a(ao[9],ay),t=a(e[7],h[5]);function
aC(b){var
d=a(C[2][1][1],b);return a(c[10],d)}var
I=b(e[17][15],aC,x),aD=aQ(0,t),aF=aE(b(c[i][4],I,D),aD),aG=aQ(0,t),aH=b(e[17][10],I,aG),aI=b(e[17][10],aH,[0,aF,0]),aK=a(aO[50],aA),aL=aE(a(c[8],aK),aI),aM=b(c[37],aL,t),aP=aB(_),aR=z(g(s[gN],[0,h[1]],aM,aP));return b(m[5],J,aR)},ac=o(e[17][23],ab,m[1],B,I),ad=[0,dD(0,0),0];if(a(H[3],q))var
J=m[1];else
var
ah=cU(b(E[17],d[1][2],xz)),J=aj(xA,a(m[21],ah));var
ae=[0,eK(d[1]),[0,J,ad]],af=cU(d[1][2]),ag=[0,ac,[0,a(m[21],af),ae]],D=a(m[7],ag)}if(0===O[0]){var
S=[0,aj(xk,z(gi(d[1]))),0],U=function(c){var
d=cV[3];function
e(a){return b(d,0,a)}var
f=fj(c),g=a(m[66][59],f),h=z(b(k[17],g,e));return a(m[21],h)},V=[0,b(m[30],U,p),S],Y=[0,aj(xl,eK(d[1])),V],Z=[0,aj(xm,D),Y],_=cU(d[1][2]),$=[0,dE,[0,a(m[21],_),Z]];return aj(xn,a(m[7],$))}var
aa=[0,dE,[0,D,[0,z(hK(0)),0]]];return aj(xo,a(m[7],aa));case
1:var
ak=h[4],al=h[2],am=h[1][2],K=eL(jP,q);if(K)var
an=K[1],M=function(a){return T(an,a)[a+1]};else
var
M=function(a){return 0};var
ap=function(a){var
c=a-1|0,e=T(ak,c)[c+1];if(e){var
f=e[1],g=b0(d,n,M(a-1|0),p,f),h=z(ft(0));return b(m[5],h,g)}return z(ft(0))},aq=function(d){var
j=a(y[7],d),k=a(y[2],d),h=b(c[3],k,j);if(9===h[0]){var
o=a(e[19][11],h[2]),i=a(e[17][b5],o),p=0<=n[1]?b(e[17][x],n[1],i)[2]:i,q=function(a){return 1-eo(a)},r=b(e[17][33],q,am);return a(z(hS(jO(a(y[2],d),r,al,p))),d)}var
l=a(f[3],xB);return g(m[24],0,l,d)};return aj(xC,b(m[8],aq,ap));case
2:var
ar=h[6],as=h[4],au=function(f){var
c=u(dg),g=b0(d,n,q,p,b(e[17][7],ar,f-1|0)[5]),h=v===c?dg[1]:l===c?a(t[2],dg):dg,i=z(h);return b(m[5],i,g)},av=b(m[8],as,au),aw=z(s[28]);return aj(xD,b(m[5],aw,av));case
3:var
h=h[2];continue;case
4:var
h=h[2];continue;default:var
ax=h[3],ay=h[2],aA=eL(jQ,q),aC=a(e[7],ay[1]),aD=function(h){var
F=a(y[7],h),G=a(y[2],h),l=b(c[3],G,F);if(9===l[0]){var
r=l[2],I=l[1],t=b(bm[54],r.length-1-1|0,r),o=t[1],J=T(t[2],0)[1],K=a(y[2],h),u=b(c[73],K,J),v=u[2],w=u[1],M=a(y[2],h),N=b(c[5],M,w),x=eJ(d[1],N,v),q=x[2],O=x[3],i=b(y[20],aC,h),A=q-n[2]|0,P=a(y[2],h),B=g(e[19][7],o,A+1|0,o.length-1-(A+1|0)|0),C=j[1][10][1],D=function(b,a){return cx(P,xj,a,b)},E=g(e[19][18],D,B,C),Q=a(j[1][10][21],E),R=function(a){return[0,[0,0,a],0]},S=[0,[0,b(e[17][15],R,Q)],1],U=jS(w,v,q,a(c[10],i)),V=a(c[10],i),W=[0,jS(I,o,q-n[2]|0,V),[0,U]],X=a(c[21],W),Y=[0,b0(d,n,aA,p,ax),0],Z=a(s[76],[0,i,0]),_=[0,aj(xF,a(k[70][8],Z)),Y],$=[0,aj(xG,z(b(s[5],X,2))),_],aa=[0,a(y[2],h),O],ab=[0,aj(xH,z(L(s[145],1,0,[0,i],aa,S))),$];return b(m[7],ab,h)}var
H=a(f[3],xE);return g(m[24],0,H,h)},aF=[0,z(gi(d[1])),0],aG=a(m[20],[0,aD,0]),aH=eK(d[1]),aI=cU(d[1][2]),aK=a(m[21],aI),aL=b(m[5],aK,aH),aM=[0,b(m[10],aL,aG),aF],aP=[0,z(s[28]),aM];return aj(xI,a(m[7],aP))}}function
eM(d,c){if(bf[1]){var
g=function(h){function
g(g){function
h(i){var
h=b(e[17][15],k[10],i),j=k7(bj[85],0,0,g,0,0,0,0,h),m=a(f[3],xJ),n=a(f[3],d),o=a(f[3],xK),p=b(f[12],o,n),q=b(f[12],p,m),r=b(f[12],q,j);b(at[10],0,r);function
s(i){var
e=i[1];if(e[1]===cz[29])var
c=e[3],n=e[2],o=k7(bj[85],0,0,g,0,0,0,0,h),j=u(c),p=a(f[3],xL),q=v===j?c[1]:l===j?a(t[2],c):c,r=a(f[13],0),s=a(f[3],d),w=a(f[3],xM),x=a(f[16],n),y=a(f[3],xN),z=b(f[12],y,x),A=b(f[12],z,w),B=b(f[12],A,s),C=b(f[12],B,r),D=b(f[12],C,q),E=b(f[12],D,p),m=b(f[12],E,o);else
var
m=a(af[17],i);var
F=a(f[3],xO),G=b(f[12],F,m);b(at[10],0,G);return a(k[16],0)}function
w(c){if(0===c){var
e=a(f[3],xP),g=a(f[3],d),h=b(f[12],g,e);b(at[10],0,h);return a(k[16],0)}return aB(function(c){var
d=a(bj[84],c),e=a(f[3],xQ),g=b(f[12],e,d);b(at[10],0,g);return[0,[0,c[1],0],c[2]]})}var
x=b(k[71][1],k[53],w),y=b(k[18],c,x);return b(k[23],y,s)}return b(k[71][1],k[64][6],h)}return b(k[71][1],k[54],g)};return b(k[71][1],k[55],g)}return c}function
jT(n,D,d,C,j,B,y){if(n){var
o=n[1];if(0===o[0]){var
f=o[1];if(f)if(!f[2]){var
aa=a(R[66],D),ab=a(A[2],0),ac=b(aA[55],ab,aa),ad=T(a(R[72],ac)[1][1],0)[1],ae=b(K[5],C,xX),af=[0,b0(d,xY,0,0,j),0],ag=[0,z(s[28]),af],ai=function(f,e){var
d=a(bF,f),h=d[3],i=d[1];function
j(n,e){var
b=u(ah),f=v===b?ah[1]:l===b?a(t[2],ah):ah,g=a(aO[50],f),d=u(ap),i=a(c[8],g),j=v===d?ap[1]:l===d?a(t[2],ap):ap,k=a(aO[50],j),m=[0,0,a(c[8],k),i,h];return[0,e,a(c[20],m)]}var
m=g(s[54],0,j,[0,i,0]);return b(k[70][8],m,e)},aj=[0,a(m[40],ai),ag],ak=[0,z(b(s[8],[0,ae],ad+1|0)),aj],al=[0,z(dh(0)),ak];return aB(a(m[7],al))}var
E=function(a){return 0===a[2][0]?1:0},p=b(e[17][35],E,f),q=p[1],F=p[2],G=function(b){var
a=b[2];return 0===a[0]?a[1][1]+1|0:-1},H=b(e[17][15],G,q),I=function(b){var
a=b[1][5];if(a)if(0===a[1][0])return 1;return 0},r=b(e[17][35],I,y),w=r[2],h=r[1],J=ge[7],M=[0,xR,[0,d[1][2],0]],i=function(c){if(c){var
d=c[2];if(d){var
e=[0,i(d),0],f=[0,a(k[16],0),e],g=a(k[37],f),h=a(s[g1],0);return b(k[71][2],h,g)}}return a(k[16],0)},x=function(c){function
g(g){var
c=d[1],h=g[3],i=d[3],j=d[2],k=c[6],l=c[5],m=b(e[18],d[1][4],g[2][4][4]),n=[0,[0,c[1],c[2],c[3],m,l,k],j,i],o=h[5];return aB(b0(n,[0,0,a(e[17][1],f)],0,0,o))}var
h=b(e[17][15],g,c),i=a(k[37],h);return b(k[71][2],s[28],i)},N=x(w),O=function(c){var
d=c[2],f=c[1];if(1===d[0]){var
e=d[1];if(e)return b(s[8],[0,f],e[1][1]+1|0)}return a(k[16],0)},Q=b(e[17][15],O,F),S=a(k[37],Q),U=b(k[71][2],S,N),V=x(h),W=hP(0),X=gh(0,H),Y=b(k[71][2],X,W),Z=b(k[71][2],Y,V),_=a(e[17][1],h),$=function(n){function
j(g,d){var
m=a(k[66][6],n),e=b(c[3],m,g);if(9===e[0]){var
f=e[2];if(2===f.length-1){var
h=f[1],i=f[2],o=e[1];if(1===d)return[0,h,[0,i]];var
l=j(i,d-1|0),p=l[2];return[0,a(c[21],[0,o,[0,h,l[1]]]),p]}}if(1===d)return[0,g,0];throw[0,P,xS]}var
d=j(a(k[66][3],n),_),f=d[2],l=d[1],o=eM(xT,L(J,0,0,0,0,M)),p=[0,a(k[16],0),0],r=a(e[17][1],q),t=g(k[32],1,r,Z),u=i(h),v=[0,eM(xU,b(k[71][2],u,t)),p],x=a(k[37],v),y=b(s[lK],0,l);if(f)var
z=f[1],A=[0,a(k[16],0),0],B=eM(xV,i(w)),C=b(k[71][2],s[16],B),D=[0,eM(xW,b(k[71][2],C,U)),A],E=a(k[37],D),F=a(c[18],[0,0,l,z]),G=b(s[lK],0,F),m=b(k[71][2],G,E);else
var
m=a(k[16],0);var
H=dh(0),I=b(k[71][2],H,m),K=b(k[71][2],I,y),N=b(k[71][2],K,x);return b(k[71][2],N,o)};return a(k[66][10],$)}}var
am=[0,b0(d,xZ,B,0,j),0],an=[0,z(s[28]),am],ao=[0,z(dh(0)),an],aq=a(m[7],ao);return aB(a(m[22],aq))}function
x0(a){function
c(d){function
c(a){return b(A[52],[0,a[1]],1)}return b(e[17][14],c,a)}return[0,c,function(d){function
c(a){return b(A[52],[0,a[1]],a[2])}return b(e[17][14],c,a)}]}function
jU(n,N,i,h,r,M,J,I){function
x(a){return hR(a)}var
O=n[4];function
Q(b){var
c=a(a0[29],b[3]);return a(au[8],c)}var
R=[0,h,[0,r,b(e[17][15],Q,O)]];function
S(a){return[0,a,0]}var
T=b(e[17][15],S,R),B=u(df),U=eN[3],W=v===B?df[1]:l===B?a(t[2],df):df,C=x0([0,[0,a(au[8],W),U],T]),d=C[2],D=C[1];function
F(c,b){a(D,0);var
e=a(c,b);a(d,0);return e}function
p(a){return F(z(hO(0)),a)}var
Y=z(s[61]);function
Z(a){return F(Y,a)}var
_=0,$=[0,b(E[17],n[2],x1),0];function
aa(a){return fs($,_,a)}var
ab=0,ac=[0,n[2],0];function
ad(a){return fs(ac,ab,a)}var
q=b(m[5],ad,aa);function
ae(d){var
k=a(y[7],d),l=a(y[2],d),j=b(c[3],l,k);if(9===j[0]){var
f=j[2];if(3===f.length-1){var
n=f[2],o=f[3],e=a(y[2],d),q=b(c[82],e,n)[1],r=b(c[82],e,o)[1],t=a(c[22],h);if(g(c[94],e,t,q))if(eh(e,i,r)){var
u=0===i[0]?[0,i[1][2]]:[1,i[1][3]],v=a(s[68],[0,[0,x3,[1,h]],[0,[0,x2,u],0]]),w=[0,p,[0,z(hL(0)),0]],x=[0,z(v),w];return b(m[7],x,d)}return a(z(s[b5]),d)}}return a(z(s[b5]),d)}var
af=z(s[b5]);function
ag(c){a(d,0);var
b=a(af,c);a(D,0);return b}var
H=aj(x4,b(m[4],ag,ae));function
w(a){return g(s[c_],0,0,a)}function
j(i,B){var
d=B;for(;;){switch(i[0]){case
0:var
r=i[2];if(0===i[4][0])switch(d[0]){case
0:var
t=d[2];if(0===d[4][0]){var
C=function(p,o,d){try{var
f=a(e[17][5],d[2]);if(0!==f[0])throw[0,P,x_];var
i=b(aJ[8][22],f[1],N)}catch(a){a=G(a);if(a===X)throw[0,P,x5];throw a}var
h=i[2],l=i[1];return function(i){var
q=a(y[8],i),f=[0,a(y[2],i)],k=a(e[7],d[5]),r=[0,l,V(0,k)],t=a(c[21],r),u=V(0,k),v=a(c[21],[0,d[7],u]),w=bi(q,f,d[6],t,v),x=b(c[37],w,k);function
n(d){var
a=b(c[82],f[1],d)[1];if(b(c[55],f[1],a))return b(c[74],f[1],a)[1];throw[0,P,x6]}var
A=n(l),B=n(d[7]);function
C(b){return a(z(a(s[68],[0,[0,x8,[1,A]],[0,[0,x7,[1,B]],0]])),b)}var
D=[0,C,[0,aj(x9,j(o[9],d[9])),0]],E=[0,z(s[28]),D],F=aB(a(m[7],E)),G=z(g(s[c_],0,[0,h],F)),H=z(a(s[78],0)),I=aB(b(m[5],H,G)),J=g(s[gN],[0,h],x,I),K=a(c[10],h),L=[0,z(b(cV[3],0,K)),[0,p,0]],M=[0,z(J),L],N=[0,a(cz[11],f[1]),M];return b(m[7],N,i)}},D=a(e[17][1],t);if(a(e[17][1],r)===D){var
E=o(e[17][23],C,m[1],r,t),F=[0,E,[0,aj(x$,a(m[21],q)),[0,p,[0,H,0]]]],I=[0,z(s[28]),F];return aj(ya,a(m[7],I))}throw[0,P,yb]}var
l=1;break;case
3:var
h=0,l=0;break;default:var
h=1,l=0}else
var
l=1;if(l)switch(d[0]){case
0:var
u=d[4],J=d[1][1];if(0!==u[0]){var
M=a(b9,b(e[17][7],J,u[1]-1|0));return z(w(x(a(K[10][16],M))))}var
h=1;break;case
3:var
h=0;break;default:var
h=1}break;case
1:var
O=i[4];switch(d[0]){case
1:var
Q=d[4],R=d[2];return aj(ye,function(d){var
r=a(y[7],d),s=a(y[2],d),i=b(c[3],s,r);if(9===i[0]){var
k=i[2];if(3===k.length-1){var
u=k[3],h=a(y[2],d),l=b(c[82],h,u),v=l[2],A=l[1];if(cp[1])var
B=b(c[78],h,A)[3],o=B,n=m[1];else
var
M=a(e[17][9],v),o=b(e[17][7],M,R-1|0),n=q;var
C=b(c[82],h,o)[1],D=b(c[67],h,C),E=a(e[19][11],O),F=function(a){return a},G=b(e[17][72],F,E),H=a(e[19][11],Q),I=function(a){return a},J=b(e[17][72],I,H),K=function(c){var
d=b(e[17][7],G,c-1|0),f=[0,n,[0,p,[0,j(d,b(e[17][7],J,c-1|0)),0]]];return a(m[7],f)},L=z(x(D));return a(z(w(aB(b(m[8],L,K)))),d)}}var
t=a(f[3],yd);return g(m[24],0,t,d)});case
3:var
h=0;break;default:var
h=1}break;case
2:return aj(yf,j(b(e[17][7],i[6],0)[5],d));case
4:var
S=j(i[2],d),T=z(hH(0));return aj(yg,b(m[5],T,S));case
5:var
U=i[3];switch(d[0]){case
3:var
h=0;break;case
5:var
v=d[2],W=d[3],Y=a(e[7],v[1]),Z=v[5],A=function(d){var
v=a(y[7],d),w=a(y[2],d),i=b(c[3],w,v);if(9===i[0]){var
h=i[2];if(3===h.length-1){var
B=h[2],C=h[3],e=a(y[2],d),l=b(c[73],e,B),D=l[2],E=l[1],o=b(c[73],e,C),F=o[2],G=o[1],I=eJ(n,b(c[5],e,E),D)[3],r=eJ(n,b(c[5],e,G),F),t=r[3],J=r[1],u=b(y[20],Y,d);if(b(aJ[3],J,Z)){var
K=[0,q,[0,j(U,W),0]],M=a(s[76],[0,u,0]),N=[0,a(k[70][8],M),K],O=[0,z(L(s[bN],0,[0,u],t,0,dv[4])),N],P=aB(a(m[7],[0,H,0])),Q=[0,z(g(cV[13],I,t,P)),O];return b(m[7],Q,d)}return b(m[7],[0,q,[0,p,[0,A,0]]],d)}}var
x=a(f[3],yh);return g(m[24],0,x,d)},_=[0,p,[0,aj(yi,A),0]],$=[0,z(s[28]),_];return z(w(aB(a(m[7],$))));default:var
h=1}break;default:var
h=0}if(!h)if(3===d[0]){var
d=d[2];continue}throw[0,P,yc]}}try{var
ah=a(s[68],[0,[0,yk,[1,h]],[0,[0,yj,[1,r]],0]]),ai=0,ak=[0,Z,[0,function(c){b(A[52],[0,h],1);b(A[52],[0,r],1);return a(j(M,J),c)},ai]],al=[0,z(ah),ak],am=[0,z(s[28]),al],an=[0,z(dh(0)),am],ao=b(m[7],an,I);a(d,0);return ao}catch(b){b=G(b);a(d,0);throw b}}function
jV(q,l,y,d,x){var
n=ge[7],p=[0,yl,[0,d[2],0]];function
h(a){return L(n,0,0,0,a,p)}function
j(f,d){function
l(l){var
r=a(k[66][5],l),n=a(k[66][6],l),A=a(k[66][3],l),p=b(c[3],n,A);if(0===f){if(1===y){var
C=aE(q,a(e[17][9],d)),t=g(U[17],r,n,C),B=0,z=function(b){var
c=a(k[66][6],b),d=a(k[66][5],b),e=o(aq[2],0,d,c,t)[1],f=[0,h(0),0],g=[0,s[61],f],i=[0,a(s[86],t),g],j=[0,a(k[64][1],e),i];return a(m[66][20],j)},D=[0,a(k[66][10],z),B];return a(m[66][20],[0,s[61],[0,s[28],D]])}var
u=aE(q,a(e[17][9],d)),v=o(aq[2],0,r,n,u),E=b(c[90],v[1],v[2])[2],F=h(0),G=s[61],H=a(s[86],u),I=b(k[71][2],H,G),J=b(k[71][2],I,F),K=[0,a(m[66][8],J),0],L=h(0),M=a(s[141],0),N=a(m[66][59],x),O=b(k[71][1],N,M),Q=b(k[71][2],O,L),R=b(k[71][2],s[16],Q),S=[0,a(m[66][8],R),K],T=a(k[37],S),V=a(s[143],E),W=b(k[71][2],s[61],s[28]),X=b(k[71][2],W,V);return b(k[71][2],X,T)}switch(p[0]){case
6:var
Y=0,Z=function(b){return j(f-1|0,[0,a(c[10],b),d])},_=[0,a(m[66][43],Z),Y];return a(m[66][20],[0,s[16],_]);case
8:var
w=p[2],$=p[4],aa=[0,j(f-1|0,[0,w,d]),0],ab=b(c[i][5],w,$),ac=[0,b(s[5],ab,2),aa];return a(m[66][20],ac);default:throw[0,P,ym]}}return a(k[66][10],l)}try{var
t=j(l,0);return t}catch(c){var
r=a(f[3],yn);return b(m[66][4],0,r)}}var
gj=[0,ws,wt,wu,wv,ww,wx,wy,wz,wA,wB,wC,wD,wE,wF,wG,wH,wI,wJ,wK,wL,wM,jJ,wN,wO];aH(1092,[0,[0,jI],gj,jK,jL,wU,dD,gf,wX,gg,wY,cU,gh,eJ,jO,dE,eK,jP,jQ,jR,eL,gi,b0,jT,jU,jV],"Principles_proofs");function
jW(c){var
a=c[5];if(a){var
b=a[1];if(0!==b[0])return b[1]?yo:yp}return 0}function
jX(a){return typeof
a==="number"?0===a?1:0:a[1]}function
jY(a){return a[1]}function
yq(a){return a[2]}function
jZ(f,d,b){function
e(h){var
a=h;for(;;){if(a<b.length-1){if(a<d.length-1){var
i=T(b,a)[a+1],j=T(d,a)[a+1];if(g(c[94],f,j,i))return[0,a,e(a+1|0)];var
a=a+1|0;continue}var
a=a+1|0;continue}return[0,a,0]}}return e(0)}function
gk(b,a){function
d(h,c,g){var
b=h,a=g;for(;;){if(c)if(a){var
e=a[2],f=c[1],i=a[1],j=c[2];if(b<f){var
b=b+1|0,a=e;continue}if(b===f)return[0,i,d(b+1|0,j,e)];throw[0,P,yr]}return a}}return d(0,b,a)}var
aW=a(ys[1],[0,R[80]]);function
j0(k,d,i){if(d){var
e=d[1];if(0===e[0]){var
l=e[1];try{var
m=function(a){var
c=a[2];return b(j[1][1],k,a[1])?[0,c]:0},a=b(F[c3],m,l);if(0===a[0])var
f=a[1][1],c=0;else{var
h=a[1];if(h)var
f=h[1][1],c=0;else
var
g=yv,c=1}if(!c)var
g=[0,f<i?1:0];return g}catch(a){a=G(a);if(a===X)return 0;throw a}}}return 0}function
j1(c,b){var
d=aW[1];function
e(e,d,b){var
f=a(c,e);return g(aW[4],f,d,b)}return g(aW[13],e,b,d)}function
gl(c,a){function
d(e,c,a){if(c){var
d=c[1];return a?[0,b(E[5],d,a[1])]:c}return a?a:0}return g(aW[8],d,c,a)}function
j2(d,c){function
e(a,e){var
c=a[1],f=a[2],g=b(a1[12],[0,d,0],c);return[0,c+1|0,[0,b(C[1][1][14],g,e),f]]}var
f=g(p[20],e,yw,c)[2];return a(p[9],f)}function
gm(t,aa,l,w,v,d,M){var
ab=l?l[1]:1,ac=a(p[1],d);function
N(a){return a[1][1]}var
B=b(p[17],N,d),H=[0,0];function
ad(o,e){function
f(f){var
l=f[5],q=f[4],m=f[3],s=f[2],u=f[1],v=u[2],y=b(r[69],t,u[1]),L=y[2],z=a(c[bO][1],y[1]),M=b(aK[15],c[bO][1],L).length-1,A=a(C[1][6],q)+M|0;if(b(R[74],z,o)){var
N=a(R[66],z)[1],O=a(p[1],e),Q=a(j[17][9],N),B=j0(a(j[6][7],Q),w,O);if(B){if(0===B[1])return 0;if(A<=a(p[1],e))var
G=0,D=b(F[x],A,e);else{var
S=b(p[17],c[bO][2],q),d=a(p[9],S),h=e;for(;;){if(h){if(d){var
n=d[1],J=h[2],K=h[1];if(0===n[0]){var
d=j2(K,d[2]),h=J;continue}var
d=j2(n[2],d[2]);continue}throw[0,P,yx]}var
k=a(p[9],d),T=a(p[1],k),U=g(C[1][13],b$[i],0,k),V=a(a1[8],T),W=b(p[17],V,e),G=k,D=[0,b(E[26],W,U),0];break}}return[0,[0,m,l,v,G,D]]}return[0,[0,m,l,v,0,[0,e,0]]]}if(s){var
H=s[1],X=H[2],Y=b(r[69],t,H[1])[1],Z=a(c[bO][1],Y),I=a(R[64],Z)[1];return b(R[74],I,o)?[0,[0,m,l,X,0,[0,e,0]]]:0}return 0}try{var
h=[0,b(F[c3],f,d)];return h}catch(a){a=G(a);if(a===X)return 0;throw a}}function
f(e,h,k,m){var
d=a(R[26],m);switch(d[0]){case
6:var
I=d[3],ai=d[2],aj=d[1];if(b(a1[3],1,I)){var
J=f(e,h,k,ai),ak=J[2],al=J[1],K=f(e,h,al,b(a1[14],R[6],I)),am=K[1],an=[0,aj,ak,b(a1[8],1,K[2])];return[0,am,a(R[10],an)]}break;case
7:var
L=d[2],M=d[1],ao=f(e+1|0,[0,[0,M,0,L],h],aW[1],d[3])[1];return[0,gl(k,j1(function(b){return a(R[10],[0,M,L,b])},ao)),m];case
8:var
N=d[3],y=d[2],O=d[1],ap=d[4],aq=f(e,h,k,y)[1],ar=f(e+1|0,[0,[0,O,[0,y],N],h],aW[1],ap)[1];return[0,gl(aq,j1(function(b){return a(R[12],[0,O,y,N,b])},ar)),m];case
9:var
P=d[2],n=d[1],as=a(c[8],n),A=b(c[3],t,as);if(10===A[0])var
Z=a(j[17][9],A[1][1]),_=a(j[6][7],Z),Q=b(j[1][10][3],_,aa);else
var
Q=0;if(!Q){var
at=function(b,a){return f(e,h,b,a)[1]},S=g(aK[17],at,k,P),z=a(aK[11],P),D=ad(n,z);if(D)var
l=D[1],o=[0,[0,l[1],l[2],l[3],l[4],l[5]]];else{if(w){var
E=w[1];if(0===E[0])var
s=0;else{var
x=E[1];if(eh(t,x,a(c[8],n)))if(0===x[0])var
o=0,s=1;else
var
ae=x[1],af=[0,a(F[b5],z),0],o=[0,[0,ac-1|0,ae[2],0,0,af]],s=1;else
var
s=0}}else
var
s=0;if(!s)var
o=0}if(o){var
u=o[1],T=u[5],U=T[1],q=u[4],au=T[2],av=u[1],aw=gk(u[3],U),ax=[0,n,a(bm[12],U)],ay=a(R[13],ax),V=b(r[16],ay,q),az=g(C[1][13],R[1],0,q),aA=a(p[1],q),aB=b(a1[8],aA,V),aC=[0,b(b$[59],aB,az)],aD=a(aK[12],aw),aE=(((av+1|0)+v|0)+e|0)+a(p[1],q)|0,aF=[0,a(R[1],aE),aD],aG=[0,a(R[13],aF),aC],aH=a(R[13],aG),aI=b(b$[69],aH,q),$=gl(b(aW[6],aI,H[1]),S);H[1]++;return[0,$,a(b$[58],[0,V,au])]}var
aJ=[0,n,a(aK[12],z)];return[0,S,a(R[13],aJ)]}break;case
13:var
W=d[4],aL=d[2],aM=d[1],X=f(e,h,k,d[3]),aN=X[2],aO=X[1],aP=function(b,a){return f(e,h,b,a)[1]},aQ=g(aK[17],aP,aO,W),aR=a(R[23],[0,aM,aL,aN,W]),aS=a(c[8],aR),aT=g(c[i][3],B,v+1|0,aS);return[0,aQ,a(c[bO][1],aT)];case
16:var
aU=d[1],Y=f(e,h,k,d[2]),aV=Y[1];return[0,aV,a(R[17],[0,aU,Y[2]])]}if(ab)var
ag=a(c[8],m),ah=g(c[i][3],B,v+e|0,ag),G=a(c[bO][1],ah);else
var
G=m;return[0,k,G]}var
O=a(c[bO][1],M),m=f(0,0,aW[1],O),n=m[2],o=m[1];function
q(b,d){var
c=a(R[41],b);return c?c:a(R[40],b)}var
e=b(aW[17],q,o),s=e[2],u=e[1];function
y(d,e,c){var
f=a(b$[82],d),h=f[2],i=a(p[1],f[1]);if(g(a1[4],1,i,h)){var
j=b(a1[8],-i|0,h);return b(aW[3],j,c)?c:g(aW[4],d,e,c)}return g(aW[4],d,e,c)}var
z=g(aW[13],y,u,s),A=a(aW[19],z);function
D(b,a){return an.caml_int_compare(b[2],a[2])}var
I=b(p[48],D,A);function
J(d,f){var
e=d[1],g=d[2],h=a(c[8],f[1]),k=b(c[i][1],e,h);return[0,e+1|0,[0,[0,[0,a(j[1][6],yu)],k],g]]}var
h=g(p[20],J,yt,I),k=h[1],K=h[2],L=a(c[8],n);return[0,K,k,b(c[i][1],k,L)]}function
j3(a,j,i,d){function
f(e,d){var
h=b(c[3],a,d);switch(h[0]){case
1:if(g(c[94],a,j,d))return g(i,e,d,[0]);break;case
9:var
k=h[1],m=h[2];if(g(c[94],a,j,k)){var
n=function(a){return a+1|0},p=o(c[fd],a,n,f,e);return g(i,e,k,b(aK[15],p,m))}break}function
l(a){return a+1|0}return L(c[fd],a,l,f,e,d)}return f(0,d)}function
j4(f,e,d,b){return j3(f,d,function(h,f,b){var
d=[0,e,g(aK[7],b,0,b.length-1-1|0)];return a(c[21],d)},b)}function
j5(d,c,b,a){return dC(function(a){return j4(d,c,b,a)},a)}function
eO(a){return b(S[1],0,[1,a])}function
j6(e,q,d){function
f(s){var
h=s;for(;;){var
d=b(c[3],e,h);switch(d[0]){case
6:var
k=d[3],m=d[2],t=d[1],o=b(c[90],e,m)[2],l=b(c[82],e,o)[1];if(b(c[46],e,l))var
p=b(c[76],e,l)[1][1],n=b(j[23][13],p,q);else
var
n=0;if(n){var
u=a(c[9],1);if(g(r[37],e,u,k))throw[0,P,yy];var
h=b(c[i][5],c[14],k);continue}var
v=[0,t,m,f(k)];return a(c[18],v);case
8:var
w=d[3],x=d[2],y=d[1],z=[0,y,x,w,f(d[4])];return a(c[20],z);default:return h}}}return b(d$,f,d)}function
j7(g,f){var
d=a(R[26],g);if(0===d[0]){var
e=d[1],h=a(bG,b(p[7],f,e-1|0));return b(c[i][1],e,h)}return c[14]}function
j8(l,e,W,T,S,R,v,k,Q,u,O,N){var
w=b(c[90],e[1],N),y=w[2],z=w[1];function
X(a){return jX(a[2][8][1])}var
Y=b(p[35],X,k),A=a(p[1],Y);if(1===A)var
Z=a(p[1],u)+2|0,q=b(F[c5],Z,z);else
var
q=z;if(1===A)var
_=b(c[i][4],[0,c[14],[0,O,0]],y),s=b(c[37],_,u);else
var
L=function(h,g,f){var
d=b(c[90],e[1],h),j=d[2],k=b(F[c5],2,d[1]),l=[0,f,V(0,g)],m=[0,a(c[21],l),0],n=b(c[i][4],[0,c[14],m],j);return b(c[37],n,k)},M=function(m,l){var
f=m,d=l;for(;;){var
g=b(c[3],e[1],f);if(d){var
i=d[1][2][8][1];if(typeof
i==="number")if(0!==i){var
d=d[2];continue}if(9===g[0]){var
h=g[2];if(2===h.length-1){var
k=d[1][2],o=g[1],p=h[1],q=k[4],r=k[1][1],s=M(h[2],d[2]),t=[0,o,[0,L(p,q,r),s]];return a(c[21],t)}}var
j=d[1][2],n=d[2],f=L(f,j[4],j[1][1]),d=n;continue}return f}},s=M(y,k);var
t=j6(e[1],R,q);if(1===v){var
aa=b(c[37],s,t);return[0,a(p[1],t),aa]}var
ab=ho(e),ac=a(p[1],q)-v|0,B=b(F[x],ac,t),ad=B[1],C=a(F[gZ],B[2]),ae=C[2],af=C[1];function
ag(I,u,t){var
f=t[2],v=f[7],w=f[6],x=f[5],m=f[4],y=f[3],J=t[1];if(1===f[8][1]){var
K=a(ak,u)[1],n=a(p[1],m),z=[0,$([0,0,0,x]),m],L=0,M=function(k,e){var
d=(n-e[2]|0)+1|0,f=e[1],g=a(bG,b(p[7],m,(d-1|0)-1|0)),h=b(c[i][1],d,g),j=a(c[9],d);return[0,d,h,b(c[i][1],1,f),j]},h=g(F[77],M,L,v),d=a(p[1],h),N=ar(e,hk(0)),A=function(f,d,l,k,h,g){var
m=b(c[i][1],1,g),n=a(c[9],1),p=b(c[i][1],1,d),q=o(r[51],e[1],p,n,m),s=[0,[0,a(j[1][6],yz)],f,q],t=[0,N,[0,f,d,a(c[19],s),h,l,k]];return a(c[21],t)};if(h){if(h[2])throw[0,P,yA];var
s=h[1],q=s[3],B=s[2],O=s[4],Q=function(f,k){var
l=k[2],n=k[1],h=b(c[i][1],d,O),t=j7(b(c[5],e[1],f),m),j=b(c[i][1],d+1|0,t);if(g(r[37],e[1],h,j)){if(b(c[44],e[1],q))var
p=b(c[i][1],d+1|0,f);else
var
z=b(c[i][1],d+1|0,f),C=a(c[9],1),D=b(c[i][1],d,q),p=A(b(c[i][1],d,B),h,D,C,z,j);if(b(c[44],e[1],q))var
s=b(c[i][1],d+3|0,f);else
var
u=b(c[i][1],2,j),v=b(c[i][1],d+3|0,f),w=a(c[9],1),x=a(c[9],2),y=b(c[i][1],2,h),s=A(b(c[i][1],d+2|0,B),y,x,w,v,u);return[0,[0,p,n],[0,[0,h,s],l]]}var
E=b(c[i][1],d+1|0,f),F=b(c[i][1],d,q);return[0,[0,o(r[51],e[1],F,h,E),n],l]},C=g(p[21],Q,w,yB),E=C[1],D=C[2]}else
var
ap=a(c[i][1],d+1|0),E=b(p[17],ap,w),D=0;var
R=b(c[i][1],(d*2|0)+1|0,x),V=[0,a(c[9],d+1|0),R],X=function(n,f){var
q=f[4],k=f[3],m=f[2],h=n[2],s=n[1],t=f[1]+(2*d|0)|0,u=a(c[9],t);if(g(r[37],e[1],u,h)){var
v=b(c[i][1],d+1|0,q),w=a(c[9],1),x=bi(l,e,b(c[i][1],d+1|0,m),w,v),y=a(c[9],1),z=a(c[9],t),A=o(r[51],e[1],z,y,h),B=b(c[i][1],1,A),C=function(b,a){return o(r[51],e[1],a[1],a[2],b)},E=g(p[20],C,B,D),F=ar(e,hl(0)),G=a(c[9],1),H=b(c[i][1],d,k),I=[0,[0,a(j[1][6],yC)],x,E],J=a(c[19],I),K=b(c[i][1],d,m),L=[0,[0,a(j[1][6],yD)],K,J],M=a(c[19],L),N=b(c[i][1],d,q),O=[0,F,[0,b(c[i][1],d,m),N,M,s,H,G]],P=a(c[21],O);return[0,P,b(c[i][5],k,h)]}return[0,s,b(c[i][5],k,h)]},Y=g(p[20],X,V,h)[1];if(y){var
Z=y[2],_=1,G=b(ha(function(b,a){return i2(a[2][3],Z)?[0,(J+1|0)-b|0]:0}),_,k);if(G){var
aa=a(c[9],G[1]),ac=aE(aE(b(c[i][1],(n+1|0)+d|0,aa),E),[0,Y,0]),ad=function(a){return bi(l,e,a[2],a[3],a[4])},ae=b(p[17],ad,h),ah=function(a){var
d=b(c[i][1],1,a[1]),f=g(U[17],l,e[1],d);return d2(n+2|0,-(I-1|0)|0,gm(e[1],W,yF,T,n,S,f)[1])},ai=b(p[17],ah,v),H=a(p[13],ai),aj=a6(d,H),al=a(p[1],H),am=b(c[i][1],al,ac),an=fu(l,e[1],am,aj),af=function(d,b){var
e=[0,[0,a(j[1][6],yE)],d,b];return a(c[18],e)},ag=g(p[21],af,ae,an),ao=b(c[38],ag,z);return $([0,K,[0,ao],b(c[37],ab,z)])}throw[0,P,yG]}throw[0,P,yH]}return u}var
ah=a(p[6],k),ai=a(p[9],ah),D=o(F[78],ag,1,ae,ai),f=Q,d=a(p[9],ad),h=0,m=0;for(;;){if(f){var
G=f[1],I=G[1];if(typeof
I==="number")switch(I){case
1:if(d){var
al=f[2],f=al,d=di(c[14],d[2]),h=h+1|0;continue}var
n=1;break;case
2:if(!G[4]){var
f=f[2];continue}var
n=0;break;default:var
n=0}else
var
n=0;if(!n)if(d){var
f=f[2],aj=[0,d[1],m],d=d[2],m=aj;continue}throw[0,P,yI]}if(d)var
am=a(p[9],d),J=[0,h,b(E[26],am,m)];else
var
J=[0,h,m];var
K=J[2],an=b(E[26],D,[0,af,0]),ao=b(E[26],K,an),ap=b(c[i][1],-h|0,s),aq=b(c[37],ap,ao),as=function(b){var
c=a(fw,b);return a(H[3],c)},at=b(p[35],as,D),au=a(p[1],at);return[0,(a(p[1],K)+au|0)+1|0,aq]}}function
gn(e,a){function
d(f,a){var
d=a[1],g=a[2];return[0,d+1|0,[0,b(bb,b(c[i][10],d,e),f),g]]}return g(p[21],d,a,yJ)}function
yK(h,e,l,d){var
m=d[9],n=d[8],o=d[7],p=d[5],q=d[4],s=d[1],i=b(aA[21],l,h),k=b(c[aY],q,h),t=fV(k,e,0,m),u=a(f[5],0),v=az(k,e,p),w=a(f[5],0),x=a(f[3],yL),y=g(r[W],i,e,n),z=a(f[3],yM),A=a(j[1][9],s),B=a(f[3],yN),C=a(f[5],0),D=g(r[W],i,e,o),E=b(f[12],D,C),F=b(f[12],E,B),G=b(f[12],F,A),H=b(f[12],G,z),I=b(f[12],H,y),J=b(f[12],I,x),K=b(f[12],J,w),L=b(f[12],K,v),M=b(f[12],L,u);return b(f[12],M,t)}function
j9(a){function
c(a){return a[7]}return b(p[17],c,a)}function
dF(c,a){return b(r[69],c,a)[2]}function
j_(e,d){var
f=[0,[0,yO,[1,b(c[74],e,d)[1]]],0];return z(a(s[68],f))}function
go(n,d,q,l,s,j,h,f,e){function
t(e,i){if(s){var
f=b(c[90],d,i),g=f[1],j=f[2];if(l)var
h=aE(e,b(c[82],d,j)[2]);else
var
k=a(p[1],g),m=b(r[9],0,k),n=a(F[b5],m),h=aE(e,b(E[26],n,0));return b(c[38],h,g)}return e}function
k(h,f,a){var
j=aa(a[1]);function
k(e,a){var
f=e[1],j=a[2],k=a[1];try{var
g=b(r[7],k,f),h=g[1],l=al(0,n,[0,d],cJ(0,n,d,h,[2,t(j,b(c[i][1],h,g[3]))],f),e);return l}catch(a){a=G(a);if(a===X)return e;throw a}}var
e=g(p[20],k,j,f);return[0,e,al(0,n,[0,d],al(0,n,[0,d],e,a),h)]}function
m(h,f,q,e){switch(e[0]){case
0:var
z=e[2],r=e[1],af=e[4],ag=e[3],A=k(h,f,r),u=A[1],ah=A[2],B=fU(z),ab=r[3],ac=r[2],ad=r[1],ae=bH(a(p[1],B),ac),D=k(h,f,[0,b(E[26],B,ad),ae,ab])[1],ai=function(e){var
l=e[9],m=e[8],n=e[7],o=e[6],q=e[5],r=e[3],s=e[2],v=e[1],j=[0,0,e[4]];function
k(f,e){var
c=e[1],d=f[2],g=f[1],i=e[2];try{var
h=t(i,a(bF,b(h9,c,d))[3]),j=[0,[0,[0,c,h],g],h_(c,h,d)];return j}catch(a){a=G(a);if(a===X)return[0,g,d];throw a}}var
h=g(p[20],k,j,f),w=h[2],x=h[1],y=Q(d,u,o),z=Q(d,u,n),A=Q(d,u,m);return[0,v,s,r,w,q,y,z,A,dC(function(a){return b(c[i][9],x,a)},l)]},aj=b(p[17],ai,z),al=jG(d,D,af);return[0,ah,aj,Q(d,D,ag),al];case
1:var
am=e[4],an=e[3],ao=e[2],I=k(h,f,e[1]),J=I[1],ap=I[2],aq=Q(d,J,a(c[9],ao)),ar=b(c[65],d,aq),as=function(a){return m(h,f,q,a)},at=a(H[16],as),au=b(aK[15],at,am);return[1,ap,ar,Q(d,J,an),au];case
2:var
av=e[6],aw=e[5],ax=e[4],ay=e[3],az=e[2],aB=k(h,f,e[1])[2],aC=function(a){var
b=a[4],c=a[3],d=a[2],e=a[1];return[0,e,d,c,b,m(h,f,q,a[5])]};return[2,aB,az,ay,ax,aw,b(p[17],aC,av)];case
3:var
aD=e[2],aF=k(h,f,e[1])[2];return[3,aF,m(h,f,q,aD)];case
4:var
L=e[1];return[4,L,m(h,f,[0,[1,L],q],e[2])];default:var
j=e[2],M=e[1],N=j[8],O=j[7],P=j[6],R=P[2],S=j[3],v=j[1],aG=e[3],aH=j[10],aI=P[1],aJ=j[5],aL=j[2],aM=v[3],aN=v[2],aO=v[1],T=k(h,f,M),l=T[1],aP=T[2],w=function(d){var
h=[0,0,0,a(p[1],d),0];function
j(o,d){var
e=d[4],g=d[3],k=d[2],l=d[1],h=a(ak,o),m=h[3],j=h[1],q=h[2];if(j){var
n=j[1];if(b(p[42],n,f))return[0,l,k,g-1|0,[0,t(b(p[38],n,f),m),e]]}var
r=b(c[i][4],e,m),s=a(c[i][4],e),u=$([0,j,b(H[16],s,q),r]),v=a(c[i][1],1),w=b(p[17],v,e);return[0,[0,g,l],[0,u,k],g-1|0,[0,a(c[9],1),w]]}var
e=g(p[21],j,d,h),k=e[2],l=e[1];function
m(a){return[0,a]}return[0,d,b(p[17],m,l),k]},aQ=k(w(O[3]),f,O)[2],U=w(N[3]),V=k(U,f,N),aR=V[2],aS=V[1],aT=j[9],aU=k(w(j[9][3]),f,aT)[2],y=a(H[2],s)?fh(0):aJ,W=[0,[0,y],q];if(a(H[2],s))var
Y=[0,0],aV=0,aW=0,aX=function(h,g,e){if(h===S)Y[1]=a(p[1],g);if(b(c[44],d,e)){var
i=b(c[65],d,e)-1|0,j=a(b9,b(p[7],M[1],i)),k=a(K[10][16],j);return b(p[42],k,f)?g:[0,Q(d,l,e),g]}return[0,Q(d,l,e),g]},aY=o(F[92],aX,aW,aV,R),aZ=a(aA[9],n),a0=function(b){var
d=a(C[2][1][1],b);return a(c[10],d)},a1=b(p[17],a0,aZ),a2=a(aK[12],a1),a3=Y[1],a4=a(p[9],aY),_=[0,a(c[12],[0,y,a2]),a4],Z=a3;else
var
a9=function(a){return Q(d,l,a)},a_=b(p[17],a9,R),a$=a(p[1],f),aa=b(F[x],a$,a_),ba=aa[2],bb=aa[1],bc=S-a(p[1],f)|0,_=[0,aE(Q(d,l,aI),bb),ba],Z=bc;var
a5=Q(d,aS,aH),a6=Q(d,l,aL),a7=Q(d,l,aM),a8=[0,[0,aO,Q(d,l,aN),a7],a6,Z,W,y,_,aQ,aR,aU,a5];return[5,aP,a8,m(U,f,W,aG)]}}return m(h,f,j,e)}function
eP(m,j,r,h,f,q,o,d){var
k=[0,aJ[8][1]];if(h){var
n=h[1];if(0===n[0]){var
s=k[1];return[0,go(m,j[1],f,0,0,[0,[1,r],0],q,o,d),s]}var
l=n[1],t=0===l[0]?a(c[10],l[1][2]):a(c[22],l[1][3]),u=j5(j[1],f,t,d),e=function(h,f,d){switch(d[0]){case
0:var
t=d[4],u=d[3],v=d[2],w=d[1],x=function(d){var
f=b(c[aY],d[4],h),l=cC(f,j[1],0,d[8]),m=l[2];j[1]=l[1];var
r=d[7],s=d[4];function
t(b){var
d=aZ(b);return a(c[10],d)}var
u=b(p[17],t,s),n=b(c[i][4],u,r),o=b(c[75],j[1],m)[1],q=e(f,n,d[9]),v=b(K[5],d[1],yP);k[1]=g(aJ[8][4],o,[0,n,v,q],k[1]);var
w=d[8],x=d[6],y=d[5],z=d[4],A=d[3],B=[0,[0,o],a(p[6],d[2])];return[0,d[1],B,A,z,y,x,m,w,q]};return[0,w,b(p[17],x,v),u,t];case
1:var
y=d[4],z=d[3],A=d[2],B=d[1],C=function(a){return e(h,f,a)},D=a(H[16],C);return[1,B,A,z,b(aK[15],D,y)];case
2:var
E=d[6],F=d[5],G=d[4],I=d[3],J=d[2],L=d[1],M=function(a){var
b=a[4],c=a[3],d=a[2],g=a[1];return[0,g,d,c,b,e(h,f,a[5])]};return[2,L,J,I,G,F,b(p[17],M,E)];case
3:var
N=d[1];return[3,N,e(h,f,d[2])];case
4:var
m=d[2],n=d[1];if(2===m[0]){var
o=m[6];if(o)if(!o[2]){var
q=o[1],r=q[4],O=q[5],P=q[3],Q=0===l[0]?yQ:[0,l[1][4]],s=e(h,f,go(h,j[1],f,0,Q,[0,[1,n],0],P,[0,[0,n,f],0],O));return r?[3,r[1],s]:s}}return[4,n,e(h,f,m)];default:var
R=d[2],S=d[1];return[5,S,R,e(h,f,d[3])]}},v=e(m,f,u);return[0,v,k[1]]}return[0,d,k[1]]}function
j$(j,e,h,f,d){var
l=d[5],m=d[4],n=d[3],w=d[2];function
k(h,l,j,n,v,u){var
m=v,d=u;for(;;)switch(d[0]){case
0:var
q=d[2],x=d[4],y=d[3],z=d[1],A=function(d){var
m=cB(d[4]),f=m[2],n=m[1],x=b(p[17],c[10],f),j=b(c[i][4],x,d[7]);try{var
u=a(p[5],d[2]);if(0!==u[0])throw[0,P,yS];var
l=b(aJ[8][22],u[1],w),v=l[1],J=l[3],K=l[2],L=dF(e,v),M=[0,[0,[0,v,jZ(e,dF(e,j),L)],K,J]],o=M}catch(a){a=G(a);if(a!==X)throw a;var
o=0}var
y=b(c[aY],d[4],h),z=k(y,d[5],j,0,yR,d[9]);function
A(a){var
h=a[3],l=a[9],m=a[8],o=a[7],p=a[6],q=a[5],r=a[4],s=a[2],j=gn(f,a[1]),d=j[1],t=b(E[26],j[2],n);if(h)var
e=h[1],u=e[3],v=e[2],k=[0,[0,g(c[i][10],d,f,e[1]),v,u]];else
var
k=0;function
w(a){return a}var
x=gc(b(c[i][10],d,f),w,m),y=g(c[i][10],d,f,p),z=g(c[i][10],d,f,q);return[0,t,g(c[i][10],d,f,s),k,r,z,y,o,x,l]}var
B=b(p[17],A,z),q=gn(f,d[5][1]),r=q[1],C=b(E[26],q[2],n),s=g(c[i][10],r,f,j);function
t(h,g){var
d=h,a=g;for(;;){if(a){var
f=a[2];if(b(c[44],e,a[1]))return[0,d,t(d+1|0,f)];var
d=d+1|0,a=f;continue}return 0}}var
D=dF(e,s),F=t(0,a(aK[11],D)),H=g(c[i][10],r,f,d[6]),I=b(p[19],bc,d[5][2]);return[0,[0,s,F],o,d[3],C,H,I,0,B]},B=b(p[17],A,q),o=al(0,h,[0,e],z,l),C=j9(q),D=o[1],F=function(b){var
c=a(b9,b);return a(K[10][16],c)},H=b(p[17],F,D),I=function(a){return g(c[i][10],1,H,a)},r=b(p[17],I,C),J=function(a){return a},L=gc(function(a){var
d=b(c[i][4],r,a);return g(U[17],h,e,d)},J,x),M=b(p[19],bc,o[2]),N=[0,2,m[2]],O=b(c[i][4],r,y);return[0,[0,o[1],j,n,M,O,j,N,L,[0,B]],0];case
1:var
R=d[4],S=0,T=function(c,a){if(a){var
d=k(h,l,j,n,m,a[1]);return b(E[26],c,d)}return c};return g(aK[17],T,S,R);case
2:var
V=d[6],W=0,Y=function(c,a){var
d=a[5],f=al(0,h,[0,e],a[3],l),g=k(h,f,j,n,[0,m[1],0],d);return b(E[26],c,g)};return g(p[20],Y,W,V);case
3:var
Z=d[2];al(0,h,[0,e],l,d[1]);var
d=Z;continue;case
4:var
m=[0,m[1],0],d=d[2];continue;default:var
f=d[2],s=d[1],_=d[3],$=f[1][2],t=al(0,h,[0,e],s,l),aa=b(p[19],bc,t[2]),ab=al(0,h,[0,e],f[9],t)[2],ac=b(p[19],bc,ab),ad=[0,dF(e,f[6][1]).length-1,0],ae=k(h,f[8],f[6][1],0,yT,_),af=f[3],ag=[0,[0,Q(e,f[9],$),af],0],ah=[0,[0,[0,[0,f[6][1],ad],0,f[4],f[8][1],f[10],ac,ag,ae],0]],ai=[0,a(c[34],f[6])];return[0,[0,s[1],j,n,aa,f[2],j,yU,ai,ah],0]}}return k(j,m,a(c[8],n),h,f,l)}function
ka(b,d){switch(b[0]){case
0:return a(c[10],b[1]);case
1:return a(c[23],[0,b[1],d]);case
2:return a(c[26],[0,b[1],d]);default:return a(c[28],[0,b[1],d])}}function
yY(e,m,N,d,M,ak,J,h,aj,I,ai,ah,ag,D,o,aq){var
s=cT(e[1]),n=a(j[1][6],e[1][2]),O=b(K[5],n,yZ),Q=a(p[1],I),R=b(r[9],0,Q);if(m)var
q=m[1],i=q[1],y=q[3],x=[0,o];else
var
i=D,y=o,x=0;var
al=a(c[34],[0,i,R]);function
k(h){var
f=h[2],i=f[8][1],k=f[4],l=f[2],m=h[1],q=f[1][1];if(typeof
i==="number")if(0===i)var
g=0;else
var
n=0,g=1;else
var
g=0;if(!g)var
n=1;if(n){var
r=l?l[1][1][1]:q,o=aQ(0,k),s=a(c[34],[0,r,o]);if(0===m)var
p=y0;else
var
y=a(E[22],m),p=b(E[17],y2,y);var
t=b(E[17],y1,p),u=a(j[1][6],e[1][2]),v=ar(d,fj(b(K[5],u,t))),w=[0,v,b(E[26],o,[0,s,0])],x=a(c[34],w);return[0,b8(N,d[1],x,k)]}return 0}if(h){var
S=h[1];if(h[2])var
z=function(f){var
b=f;for(;;){var
c=a(F[gZ],b),d=c[2],e=k(c[1]);if(e)return[0,e[1],d];var
b=d;continue}}(h),T=z[2],U=z[1],V=function(j,f){var
g=k(j);if(g){var
b=fm[9],e=u(b),m=g[1],h=v===e?b[1]:l===e?a(t[2],b):b,i=[0,ar(d,h),[0,m,f]];return a(c[21],i)}return f},B=g(p[21],V,T,U);else
var
ap=k(S),B=a(H[7],ap);var
W=function(ay,r,aw){var
o=a(A[2],0);g(aI[22],0,[0,e[1][2],0],[1,[0,[0,0,s,[0,r]],0]]);var
f=e[1],k=a(j[1][6],f[2]),q=a(p[1],ai);if(1<q)var
u=0;else
if(0===cu(0))var
af=eO(b(K[5],k,yX)),l=b(cr[3],0,af),u=1;else
var
u=0;if(!u)var
l=ag;var
Q=a(A[2],0),v=b(A[48],Q,l),x=v[2],y=v[1],R=a(A[2],0);d[1]=a(w[17],R);if(cT(f)){var
S=f[1],T=a(A[2],0),U=dm(b(A[48],T,S)[2])[2],V=dm(x)[2],W=b(aP[40][7],U,V);d[1]=L(w[lY],0,0,w[aY],d[1],W);var
z=a(aP[37][4],x),X=b(a1[24],z,y),C=ka(l,a(c[2][1],z)),B=X}else
var
C=ka(l,c[2][3]),B=y;var
Y=a(c[8],B),D=j8(o,d,f[6],M,ak,ah,q,h,aj,I,al,Y),E=D[2],Z=D[1];function
_(x,m,v){var
d=a(A[2],0),n=a(w[17],d),o=a(aN[34],k),g=a5(n,a(ao[9],o)),h=g[2],i=a5(g[1],m),j=i[2],l=hq(i[1]),e=l[1],p=l[2],q=b6(Z),r=[0,a(c[8],q),[0,j,0]],s=[0,h,[0,L(ax[2],0,0,d,e,j),r]],t=[0,L(ax[2],0,0,d,e,h),s],u=b(K[6],yV,k);da(u,cT(f),e,0,p,t);return 0}var
$=jV(C,q,a(p[1],J),f,r);g(fk,a(A[2],0),d,E);var
aa=[0,a(bV[1],_)],ab=[0,f[3]],ac=a(w[br],d[1]),ad=b(c[5],d[1],E),ae=b(K[5],k,yW);bL(aT[7],ae,0,ad,ac,0,0,ab,[0,$],0,aa,0,[0]);var
am=a(w[17],o),an=a(aN[34],n),F=a5(am,a(ao[9],an)),t=F[2],G=a5(F[1],r),H=G[2],N=hp(G[1]),i=N[1],ap=N[2],aq=[0,t,[0,L(ax[2],0,0,o,i,H),[0,H,0]]],ar=[0,L(ax[2],0,0,o,i,t),aq];da(b(K[6],y3,n),s,i,0,ap,ar);var
O=dT[1];if(O){var
as=eN[3],at=[0,b(c[74],i,t)[1]];b(A[52],at,as);if(m){var
au=eN[3],av=[0,b(c[74],i,m[1][1])[1]];return b(A[52],av,au)}var
P=0}else
var
P=O;return P};if(s)var
C=d[1];else
var
an=a(A[2],0),C=a(w[17],an);var
X=a(w[br],C);try{var
ab=[0,a(bV[1],W)],ac=[0,jT(M,b(c[5],d[1],i),e,n,y,x,J)],ad=[0,e[1][3]],ae=b(c[5],d[1],B);bL(aT[7],O,0,ae,X,0,0,ad,ac,0,ab,0,[0]);var
am=0;return am}catch(c){c=G(c);var
Y=b(af[16],0,c),Z=a(f[5],0),_=a(f[3],y4),$=b(f[12],_,Z),aa=b(f[12],$,Y);return b(at[8],0,aa)}}throw[0,P,y5]}function
dG(B,f,i,h,n){var
t=a(p[5],n),k=t[3],C=t[2],D=t[1],P=j[1][10][1];function
Q(c,a){return b(j[1][10][7],a[2][4][6],c)}var
V=g(p[20],Q,P,n),u=k[3],v=k[1],e=C[4],G=D[6],x=C[1],W=k[5],X=k[2],Y=D[2];function
Z(a){var
b=a[3],c=a[1];return[0,c,b,j$(f,i,h,[0,jW(c),0],b)]}var
_=b(p[17],Z,n);function
I(a){function
c(a,f){var
c=a[9],d=a[7],p=f[2],q=f[1],g=a[8],h=a[6],i=a[5],j=a[4],k=a[3],l=a[2],m=a[1];if(c)var
n=c[1],o=function(a){var
c=a[7],e=a[6],f=a[5],g=a[4],h=a[3],i=a[2],j=a[1],b=I(a[8]);return[0,[0,[0,j,i,h,g,f,e,c,d],b[1]],b[2]]},e=b(F[a4],o,n);else
var
e=0;return[0,[0,[0,m,l,k,j,i,h,d,g],q],b(E[26],e,p)]}return g(p[21],c,a,y6)}function
aa(e,k){var
g=e[2],d=e[1],i=I(e[3]),l=i[2],m=i[1];if(h)var
f=h[1],j=[0,[0,[0,f[1],0],f[2],f[3]]];else
var
j=0;var
n=[0,jW(d),0],o=b(p[19],bc,g[4][2]),q=d[3],r=d[2],s=[0,[1,d[1]],0],t=[0,[0,[0,a(c[8],g[3]),0],j,s,r,q,o,0,n],m];return[0,t,b(E[26],l,k)]}var
J=g(p[21],aa,_,0);function
ab(a){return a[1]}var
L=b(p[17],ab,J),M=a(p[1],L),ac=1;function
ae(f,a){var
d=a[2],e=a[1],g=a[5],h=a[4],j=e[2],k=b(r[60],i,e[1]),l=d?[0,d[1][1]]:0;return[0,[0,k,j],l,M-f|0,h,b(c[5],i,g)]}var
N=g(F[77],ae,ac,L),d=[0,i],q=cT(e);function
af(ac,h){var
e=h[1],i=e[2],l=h[2],n=e[8],q=e[7],t=e[6],v=e[5],w=e[4],x=e[3],y=e[1];if(i)var
j=i[1][1],k=[0,j,0,x,w,v,t,q,n],J=j[2];else
var
k=e,J=y[2];function
z(e){var
h=e[8],n=e[7],q=e[5],t=e[4],v=e[3],i=e[2],j=e[1],K=n[2],L=n[1];if(v)var
w=v[1],O=w[1],P=hc(w[2]),y=O,x=b(cV[3],0,P);else{var
$=a(c[8],u);if(g(c[94],d[1],i,$))var
aa=a(c[8],u),ab=aB(j_(d[1],aa)),I=b(m[66][12],s[b5],ab);else
var
I=m[66][2];var
y=i,x=I}var
z=aE(y,t);if(0===h[0])var
A=bi(f,d,q,z,g(U[17],f,d[1],h[1]));else
var
_=[0,hy(d),[0,q,z]],A=a(c[21],_);var
B=b(c[37],A,j);o(aq[3],0,f,d,B);if(0===h[0]){var
Q=h[1],C=a(p[1],j),R=g(U[17],f,d[1],Q),k=gm(d[1],V,0,G,C,N,R),l=k[2],S=k[3],T=k[1],D=a(c[9],(C+(M-ac|0)|0)+l|0);if(K)var
F=D;else
var
Z=b(r[60],d[1],i),F=aE(D,gk(J,fv(l,b(c[82],d[1],Z)[2])));var
W=fv(l,t),X=aE(F,b(E[26],W,[0,S,0])),Y=fu(f,d[1],X,T),H=[0,hU(d[1],Y,j)]}else
var
H=0;return[0,L,x,B,H]}return[0,k,b(p[17],z,l)]}var
O=g(F[77],af,0,J),ag=0;function
ah(b,a){var
c=a[2],d=a[1],e=1;function
f(b,a){return[0,b,a]}return[0,b,d,g(F[77],f,e,c)]}var
l=g(F[77],ah,ag,O);function
ai(a){return a[2]}var
aj=b(p[17],ai,O),ak=a(p[13],aj),y=[0,gj[1]];function
al(e){var
k=e[3],h=e[2],l=h[5],i=h[4],m=e[1],u=h[3];if(0===m)var
n=y7;else
var
M=a(E[22],m),n=b(E[17],y_,M);var
j=b(K[5],v,n);function
w(b){var
c=a(b9,b),d=a(K[10][16],c);return a(R[2],d)}var
x=b(p[19],w,i);y[1]=g(gj[4],u,[0,j,x],y[1]);function
z(e){var
f=e[2][4],g=a(c[5],d[1]);return b(H[16],g,f)}var
A=b(F[72],z,k);function
B(c){var
d=c[2],e=d[4],f=d[1],g=c[1];function
h(h){var
c=a(E[22],g),d=1===f?y8:y9,e=b(E[17],d,c);return b(K[5],j,e)}return b(H[16],h,e)}var
C=b(F[72],B,k);if(0===cu(0))var
q=c[14];else
var
I=[0,$([0,0,0,l]),i],J=d[1],r=[0,f,aP[5]],s=function(e,d){var
f=d[1],g=d[2],h=a(bG,e),i=o(ax[3],0,f,J,h),j=a(kb[14],i),k=b(aP[12],j,g);return[0,b(c[bu],e,f),k]},t=g(p[21],s,I,r)[2],L=a(kb[15],t),q=a(c[13],L);var
D=a(c[18],[0,0,l,q]),G=b(c[37],D,i);return[0,j,b(c[5],d[1],G),0,C,A]}d[1]=a(w[ld],d[1]);if(!q){var
an=a(w[148],d[1]);b(bg[14],0,an);var
ao=a(A[2],0);d[1]=a(w[17],ao)}function
am(i){var
k=i[3],t=i[1],D=bK(a(p[1],k),0);if(0===t)var
o=v;else
var
C=a(E[22],t),H=b(E[17],zk,C),o=b(K[5],v,H);function
r(k){var
i=k[2],H=k[1],ao=i[4],r=i[3],v=i[2],C=a(E[22],H),I=b(E[17],zj,C),J=b(K[5],o,I);function
L(av,I,au){if(0===ao)g(ad[41],0,1,I);else{var
ar=a(A[2],0),as=[0,g(aO[31],0,ar,I),1,0],at=[0,b(S[1],0,as),0];b(ci[1],e[2],at)}var
J=H-1|0;T(D,J)[J+1]=1;function
ap(a){return a}var
L=b(bm[34],ap,D);if(L){g(ad[32],[1,x],0,0);if(h){var
aq=[0,b(c[74],d[1],h[1][1])[1]];b(A[52],aq,1)}b(A[52],[0,x],1);var
M=B?(t+1|0)===a(p[1],l)?1:0:B;if(M){var
i=b(p[17],al,l),m=b(w[mc],q,d[1]);g(cq[11],za,y$,0);var
k=g(ji[2],[0,0,0,0,i,m,0],aO[8],0);g(cq[11],zc,zb,1);var
s=cu(0);switch(s){case
0:var
o=zd;break;case
1:var
o=zh;break;default:var
o=zi}var
P=0,Q=function(d,c){var
e=1===a(p[1],i)?o:ze,f=b(K[5],c[1],e);return[0,b(S[1],0,f),0,[0,k,d],s]},v=g(F[77],Q,P,i);b(kc[5],0,v);if(1===a(p[1],i))var
R=b(E[17],zf,o),U=a(j[1][6],e[2]),V=eO(b(K[5],U,R)),z=b(cr[3],0,V);else{var
af=a(j[1][6],e[2]),C=b(K[5],af,zg),ag=function(b,a){var
c=b[1];return[0,c,jX(a[2][8][1])]},ah=g(p[23],ag,v,l),ai=function(a){var
b=a[1];return a[2]?[0,b]:0},aj=b(F[72],ai,ah),am=b(S[1],0,C);b(kc[8],am,aj);var
an=eO(C),z=b(cr[3],0,an)}switch(m[0]){case
0:var
r=a(c[25],[0,k,0]);break;case
1:var
$=a(aP[36][4],m[1]),aa=[0,[0,k,0],a(c[2][1],$)],r=a(c[26],aa);break;default:var
ab=a(aP[38][4],m[1]),ac=a(aP[36][4],ab),ae=[0,[0,k,0],a(c[2][1],ac)],r=a(c[26],ae)}var
Z=function(b,a){var
c=a[5],d=1;function
f(a,c){return[0,aI[4],q,1,0,[0,[3,[0,[0,k,b],a]]]]}var
h=[0,g(F[77],f,d,c)];return g(aI[22],0,[0,e[2],0],h)};b(p[16],Z,i);var
_=[0,e,y[1],X];return yY(_,h,f,d,G,N,n,l,ak,Y,i,k,z,a(c[8],u),W,r)}var
O=M}else
var
O=L;return O}var
M=[0,z(hQ([1,x])),0],O=[0,z(v),M],P=[0,z(s[28]),O],Q=a(m[7],P);if(!q){var
$=a(A[2],0);d[1]=a(w[17],$)}var
R=[0,a(bV[1],L)],U=[0,aB(Q)],V=[0,e[3]],Z=a(w[br],d[1]),_=b(c[5],d[1],r);bL(aT[7],J,0,_,Z,0,0,V,U,0,R,0,[0]);return 0}return b(p[15],r,k)}return b(p[15],am,l)}aH(1096,[0,jY,yq,jZ,gk,j0,gm,j3,j4,j5,eO,j6,j7,j8,gn,yK,j9,dF,j_,go,eP,dG,j$],"Principles");function
kd(b,a,e,f,d,c){if(hX(b,a,e,d)){var
g=d0(b,a,d,c);return[0,bi(b,a,e,f,c),g]}var
h=fp(b,a,d,c);return[0,fo(b,a,e,f,d,c),h]}function
ke(m,z,d,l,w,k,j,h,f,v,u){var
A=a(B[1],0),C=a(e[17][1],f),D=b(y[19],m,d),E=a(c[10],d);if(w)var
F=a(c[9],1),G=b(c[i][1],1,h),I=a(y[8],m),n=kd(b(c[x],k,I),z,G,F,D,E),J=n[1],K=[0,n[2]],L=[0,0,J,b(c[i][1],1,l)],p=a(c[18],L),o=K;else
var
p=l,o=[0];var
r=[0,0,1];function
s(e,a){var
d=a[2],f=a[1];return[0,[0,b(c[i][1],d,e),f],d+1|0]}var
t=g(e[17][19],s,f,r)[1];function
M(a){return[0,0,a]}var
N=b(e[17][15],M,t),O=b(c[i][1],C,p),P=b(c[37],O,N),Q=ay([0,d],j,h),R=b(c[35],Q,P),S=b(c[37],R,k),T=[0,a(c[11],A),2,S],U=a(c[17],T),V=[0,U,a(e[19][12],v)],q=a(c[21],V),W=[0,q,[0,a(c[10],d)]],X=a(c[21],W);function
Y(a){return q}var
Z=g(H[24],Y,X,j),_=[0,Z,a(e[19][12],u)],$=[0,a(c[21],_),o];return a(c[21],$)}function
kf(k,i,d,h,c){if(a(j[1][10][2],c))return 0;var
e=[0,c,0];return g(d_,function(f,e){var
d=f[2],a=f[1],c=aZ(e);if(b(j[1][10][3],c,h))return[0,a,d];if(b(j[1][10][3],c,a))return[0,a,[0,c,d]];var
l=g(r[99],k,i,e),m=j[1][10][1],n=b(j[1][10][9],l,a);return b(j[1][10][11],n,m)?[0,a,d]:[0,b(j[1][10][4],c,a),[0,c,d]]},e,d)[2]}var
gp=[e9,zl,e4(0)];function
gq(f,d,c){var
a=[0,d];try{var
h=function(c){var
d=cx(f,zm,j[1][10][1],c),e=a[1];function
h(c,a){if(b(j[1][10][3],c,a))throw gp;return b(j[1][10][4],c,a)}a[1]=g(j[1][10][15],h,d,e);return 0};b(e[19][13],h,c);var
i=1;return i}catch(a){a=G(a);if(a===gp)return 0;throw a}}function
kg(f,h){var
d=f[2];b(y[20],h,f);var
i=a(bF,b(y[18],f,h)),k=i[2],q=i[3];if(k)var
l=b(c[82],d,k[1]),m=l[1],g=l[2];else
var
p=b(c[82],d,q),m=p[1],g=p[2];if(0===g)return 0;var
n=d5(d,m,a(e[19][12],g)),o=n[2];if(gq(d,cx(d,zn,j[1][10][1],n[1]),o)){var
r=function(a){return 1-b(c[45],d,a)};return b(e[19][32],r,o)}return 1}function
kh(d,N,A,n,M,z,x){var
f=a(y[2],d),m=[0,f],C=a(y[8],d),D=a(y[7],d);if(A)var
p=A;else
var
ae=a(c[10],n),p=g(r[37],f,ae,D);var
q=[0,j[1][10][1]];function
O(h,k){var
l=h[10],p=h[9],s=h[8],w=h[7],x=h[6],y=h[5],z=h[4],A=h[3],t=h[2],C=o(U[66],l,f,1,h[1]),D=C[2],E=a(ak,a(e[17][5],C[1])),n=E[3],F=E[1],S=b(cy,d,k),T=L(dZ[5],0,0,0,zp,l),G=g(B[65],T,m,S),H=a(e[17][1],t),I=b(c[i][1],H,G),J=o(r[54],f,1,I,n),K=b(c[3],f,k);if(1===K[0]){var
u=K[1];if(J)if(!b(j[1][10][3],u,s)){var
af=b(j[1][10][6],u,p),ag=b(j[1][10][4],u,s),ah=a(c[21],[0,z,[0,k]]);return[0,b(c[i][5],k,D),t,A,ah,y,x,w,ag,af,l]}}var
R=F?F[1]:a(j[1][6],zo),v=hf(q[1],R,d);q[1]=b(j[1][10][4],v,q[1]);var
M=[0,[0,v],n],N=[0,M,t],V=[0,a(c[9],1)],W=[0,b(c[i][1],1,z),V],X=a(c[21],W),Z=a(e[17][1],N),O=b(c[i][1],Z,k),Y=[0,k,y];if(J)var
_=d0(l,m,b(c[i][1],-H|0,n),k),$=a(c[9],1),Q=bi(l,m,b(c[i][1],1,n),$,O),P=_;else
var
ad=fp(l,m,G,k),ae=a(c[9],1),Q=fo(l,m,b(c[i][1],1,n),ae,I,O),P=ad;var
aa=[0,Q,hA(x)],ab=cx(f,0,p,k),ac=b(j[1][10][7],ab,p);return[0,D,N,b(c[bu],M,A),X,Y,aa,[0,P,w],s,ac,l]}var
E=d5(f,z,x),k=E[2],s=E[1];if(gq(f,cx(f,zq,j[1][10][1],s),k)){var
P=function(d,a){return 1-b(c[45],f,a)},F=b(e[19][39],P,k);if(F)var
G=b(e[19][54],F[1],k),Q=G[2],u=1,l=a(c[21],[0,s,G[1]]),t=Q;else
var
u=0,l=s,t=k}else
var
u=1,l=z,t=x;if(u){var
R=j[1][10][1],S=j[1][10][1],T=[0,b(cy,d,l),0,C,l,0,0,0,S,R,C],h=g(e[19][17],O,T,t),v=h[4],H=h[2],V=h[9],W=h[8],X=h[6],Y=h[5],Z=h[3],_=a(e[17][9],h[7]),$=a(e[17][9],Y);if(N)var
aa=b(j[1][10][4],n,W),ab=a(y[9],d),ac=a(y[2],d),I=kf(a(y[8],d),ac,ab,aa,V);else
var
I=0;if(M)var
K=[0,v],J=L(ax[2],0,0,Z,w[16],v);else
var
K=0,J=v;var
ad=a(e[17][1],H)+1|0;return[0,[0,ke(d,m,n,D,p,H,K,J,X,$,_),p,ad,I]]}return 0}function
zr(o,n,f,d){var
D=o?o[1]:1,E=n?n[1]:0;a(aC[3],zs);var
p=d[2],q=b(y[20],f,d),r=a(bF,b(y[18],d,f)),t=r[2],F=r[3];if(t)var
u=b(c[82],p,t[1]),w=u[1],i=u[2],v=1;else
var
C=b(c[82],p,F),w=C[1],i=C[2],v=0;if(0===i)return a(m[1],d);var
x=kh(d,D,E,f,v,w,a(e[19][12],i));if(x){var
h=x[1],j=h[4],A=h[3],B=h[1];if(h[2])var
G=a(c[10],q),H=[0,z(b(s[lV],zt,G)),0],I=z(s[16]),J=[0,b(m[26],A,I),H],K=a(s[82],[0,[0,f,q],0]),L=[0,a(k[70][8],K),J],M=[0,a(y[39],B),L],l=a(m[7],M);else
var
O=z(s[16]),P=[0,b(m[26],A,O),0],Q=[0,z(a(s[75],[0,f,0])),P],R=[0,a(y[39],B),Q],l=a(m[7],R);if(0===j)return a(l,d);var
N=function(d){var
e=0;function
f(d){var
e=a(c[10],d),f=z(b(s[lV],zu,e));return a(m[21],f)}var
g=[0,b(m[30],f,j),e],h=a(s[83],j),i=[0,a(k[70][8],h),g];return b(m[19],i,d)};return g(m[5],l,N,d)}return a(m[1],d)}function
gr(l,f,d){var
r=l?l[1]:1,h=d[2],m=b(y[17],d,f),i=b(c[3],h,m);if(9===i[0])var
C=d5(h,i[1],i[2])[2],n=a(e[19][11],C);else
var
n=0;function
o(e){var
f=b(c[3],h,e);if(1===f[0])return f[1];var
i=a(y[2],d),k=a(y[8],d),l=g(aV[8],k,i,e),m=a(j[1][6],l);return b(y[20],m,d)}var
t=a(y[8],d);function
u(f,b){var
h=b[3],i=b[2],j=b[1],k=f[1],l=a(y[2],d),e=L(ki[6],t,l,zv,j,k),m=e[2];return[0,g(c[39],i,h,e[1]),m]}function
v(a){var
c=b(cy,d,a);return[0,a,o(a),c]}var
p=b(e[17][17],v,n),q=r?[0,[0,f,o(f),m],p]:p,w=a(y[2],d),x=[0,a(y[7],d),w],z=g(e[17][18],u,x,q)[1],A=aE(z,b(e[17][17],e[7],q)),B=b(s[5],A,2);return b(k[70][8],B,d)}function
kj(D,d){var
F=d[1],G=a(ao[41],[2,d]),s=a(A[27],d),k=s[2],t=s[1],H=t[1];function
I(b,d){return a(c[25],[0,F,b])}var
J=b(e[19][16],I,H),M=a(e[19][11],J),N=a(e[17][9],M),m=t[6],h=b(e[17][15],c[W],k[2]),O=a(e[17][1],h)-m|0,u=b(e[17][x],O,h),v=u[2],l=u[1],n=a(e[17][1],l),P=V(0,h),Q=[0,a(c[25],d),P],p=a(c[21],Q),R=a(A[2],0),f=[0,a(w[17],R)],S=a(A[2],0),T=g(B[11],0,S,f),U=b(c[37],T,[0,[0,0,p],l]),q=k[9].length-1,X=k[9],Y=k[4];function
Z(g,I,o){var
p=a(c[8],o),r=b(c[i][4],N,p),k=b(c[90],f[1],r),l=k[1],s=b(c[82],f[1],k[2])[2],t=b(e[17][x],m,s)[2],h=a(e[17][1],l)-m|0,n=a6(g+1|0,b(e[17][x],h,l)[1]),u=V(0,n),w=V((h+g|0)+1|0,v),y=b(e[19][5],w,u),z=[0,a(c[27],[0,d,g+1|0]),y],A=[0,a(c[21],z),0],B=b(e[18],t,A),C=aE(a(c[9],(h+g|0)+1|0),B),D=a(c[9],(1+q|0)-g|0),F=b(c[37],C,n),G=a(E[22],g),H=b(E[17],zw,G);return[0,[0,[0,a(j[1][6],H)],F],D]}var
r=g(e[19][58],Z,Y,X),$=a(A[2],0),aa=g(_[76],$,d,4);function
y(f){var
g=V(0,l),h=V((n+q|0)+f|0,v),i=b(e[19][5],h,g),j=[0,a(c[25],d),i];return a(c[21],j)}var
z=[0,[0,0,y(2+n|0)],l],ab=V(0,z),ac=[0,a(c[9],(n+q|0)+3|0),ab],ad=a(c[21],ac),ae=b(c[38],ad,z);function
af(a){return a[2]}var
ag=b(e[19][15],af,r),ah=[0,aa,ae,a(c[9],1),ag],ai=a(c[30],ah),aj=y(1),ak=f[1],al=a(A[2],0),am=o(aV[10],al,ak,aj,0),an=1+(r.length-1)|0,ap=[0,[0,[0,a(j[1][6],zx)],U],h];function
aq(a){return a[1]}var
ar=b(e[19][15],aq,r),as=a(e[19][11],ar),at=a(e[17][9],as),au=b(e[18],at,ap),av=[0,[0,am,b(c[i][1],an,p)],au],aw=b(c[38],ai,av),ax=b(w[c_],D,f[1]),ay=b(c[5],f[1],aw),az=aF(bg[2],0,0,0,0,[0,ax],0,ay),aA=b(K[5],G,zy),aB=[1,L(bg[3],0,0,aA,0,[0,[0,az],zz])],C=a(A[2],0);return[0,C,a(w[17],C),h,p,aB]}function
kk(w,l,f,k){var
g=k[1],d=kj(f,g),h=d[3],m=d[5],n=d[4],o=d[2],p=d[1],q=a(ao[41],[2,g]),e=[0,o],r=b(K[6],zA,q),s=hr(e),i=ar(e,m),t=L(ax[2],0,0,p,e[1],i),j=V(0,h),u=[0,a(c[21],[0,i,j]),0],v=[0,n,[0,fD(l,t,j),u]];return da(r,f,e[1],h,s,v)}function
zB(d,c,b,a){kk(d,c,b,a);return 0}cb([0,zC,function(a,b){return ca(zB,a,b)}]);function
kl(i,f,d){var
q=i?i[1]:1,l=a(y[8],d),h=a(y[2],d),t=b(cy,d,f),u=a(y[9],d),v=a(r[77],u),w=a(j[1][10][35],v),m=b(c[3],h,f),x=9===m[0]?a(e[19][11],m[2]):0;function
n(d){var
e=b(c[3],h,d);if(1===e[0])return e[1];var
f=g(aV[8],l,h,d),i=a(j[1][6],f);return b(aV[25],i,w)}function
z(e,b){var
f=b[3],h=b[2],i=b[1],j=a(y[2],d),k=L(ki[6],l,j,zD,i,e)[1];return g(c[39],h,f,k)}function
A(a){var
c=b(cy,d,a);return[0,a,n(a),c]}var
o=b(e[17][17],A,x),p=q?[0,[0,f,n(f),t],o]:o,B=a(y[7],d),C=g(e[17][18],z,B,p),D=aE(C,b(e[17][17],e[7],p)),E=b(s[5],D,2);return b(k[70][8],E,d)}function
km(h,g){var
a=b(c[3],h,g);switch(a[0]){case
10:var
d=a[1];return[0,[1,d[1]],d[2]];case
11:var
e=a[1];return[0,[2,e[1]],e[2]];case
12:var
f=a[1];return[0,[3,f[1]],f[2]];default:throw[0,bd,zE]}}function
cW(d,h,f){if(!b(c[45],d,f))if(!b(c[44],d,f)){var
i=b(c[3],d,h),j=b(c[3],d,f);if(9===i[0])if(9===j[0]){var
k=j[2],l=i[2],m=i[1];if(g(c[95],d,m,j[1]))if(b(c[56],d,m)){var
p=b(c[77],d,m)[1],e=a(_[47],p);if(e<=l.length-1)if(e<=k.length-1){var
q=g(bm[7],l,l.length-1-e|0,e),r=g(bm[7],k,k.length-1-e|0,e),s=function(a,b){return cW(d,a,b)};return g(bm[35],s,q,r)}var
t=function(a,b){return cW(d,a,b)};return o(c[99],d,t,h,f)}}var
n=function(a,b){return cW(d,a,b)};return o(c[99],d,n,h,f)}return 1}function
zF(x,q){var
A=a(y[8],q),ae=b(y[19],q,x),d=[0,a(y[2],q)];function
I(e,a,d,c,b){var
f=d4(b,a),g=d4(c,a);return L(eI[4],e,0,d,g,f)}var
w=0,h=0,n=a(c[10],x),r=ae;for(;;){var
C=b(c[3],d[1],r);if(6===C[0]){var
J=C[3],K=C[2],af=C[1],M=b(c[3],d[1],K);if(9===M[0]){var
k=M[2],V=k.length-1,D=M[1];if(3===V){var
E=k[2],N=k[3],W=u(cs),am=k[1],an=v===W?cs[1]:l===W?a(t[2],cs):cs;if(g(c[a4],d[1],an,D)){var
ao=a(e[17][1],h);if(o(c[i][14],d[1],1,ao,E))var
S=1;else{var
au=a(e[17][1],h);if(o(c[i][14],d[1],1,au,N))var
S=1;else
var
R=1,S=0}if(S){var
ap=km(d[1],D)[2],aq=a(e[17][1],h);if(o(c[i][14],d[1],1,aq,E))var
F=E,O=N;else
var
F=N,O=E;var
X=u(ct),ar=v===X?ct[1]:l===X?a(t[2],ct):ct,as=[0,dk(d[1],[0,ar,ap]),[0,am,F]],Y=a(c[21],as);if(cW(d[1],F,O))if(I(A,h,d,O,F)){var
at=b(c[i][5],Y,J),w=1,n=a(c[21],[0,n,[0,Y]]),r=at;continue}var
p=1,R=0}}else
var
R=1;if(R)var
p=0}else
if(4===V){var
Z=k[1],G=k[2],_=k[3],P=k[4],$=u(by),av=v===$?by[1]:l===$?a(t[2],by):by;if(g(c[a4],d[1],av,D)){var
aw=a(e[17][1],h);if(o(c[i][14],d[1],1,aw,G))var
U=1;else{var
aD=a(e[17][1],h);if(o(c[i][14],d[1],1,aD,P))var
U=1;else
var
T=1,U=0}if(U){var
ax=km(d[1],D)[2],az=a(e[17][1],h);if(o(c[i][14],d[1],1,az,G))var
aa=Z,H=G,Q=P;else
var
aa=_,H=P,Q=G;var
ab=u(bz),aA=v===ab?bz[1]:l===ab?a(t[2],bz):bz,aB=[0,dk(d[1],[0,aA,ax]),[0,aa,H]],ac=a(c[21],aB);if(cW(d[1],H,Q))if(I(A,h,d,Z,_))if(I(A,h,d,Q,H)){var
aC=b(c[i][5],ac,J),w=1,n=a(c[21],[0,n,[0,ac]]),r=aC;continue}var
p=1,T=0}}else
var
T=1;if(T)var
p=0}else
var
p=0}else
var
p=0;if(!p)if(!w){var
ag=d4(K,h),ah=ck(B[7],A,d,0,0,0,0,0,0,ag),ai=[0,a(c[9],1)],aj=[0,b(c[i][1],1,n),ai],al=a(c[21],aj),w=0,h=[0,ay(af,[0,ah],K),h],n=al,r=J;continue}}var
aE=b(B[41],d[1],h),aF=function(f){var
e=a(ak,f),g=e[2],h=e[3],i=e[1];if(g)if(b(c[47],d[1],g[1]))return[0,i,h];return f},ad=b(e[17][15],aF,aE),aG=b(c[37],r,ad),aH=b(c[38],n,ad),aI=b(B[35],d[1],aH),aJ=b(B[35],d[1],aG);if(w){var
aK=z(a(s[42],aI)),aL=z(b(s[lb],x,aJ));return g(m[9],aL,aK,q)}var
aM=a(j[1][9],x),aN=a(f[3],zG),aO=b(f[12],aN,aM);return g(m[24],0,aO,q)}}function
kn(c,b){try{a(z(a(s[75],[0,c,0])),b);var
h=0,d=h}catch(b){b=G(b);if(!a(af[20],b))throw b;var
d=1}if(d){var
e=a(f[3],zH);return g(m[24],0,e,b)}return zF(c,b)}function
gs(p,n){function
d(h){var
u=a(k[66][5],h),v=a(aA[41],u),w=a(k[66][4],h),l=n[2],q=n[1];function
x(b){var
c=a(C[2][1][1],b);return a(r[e6],c)}var
s=b(F[aY],x,w),o=s[1],d=b(c[aY],s[2],v);function
y(w){var
r=cB(o),n=r[1],x=r[2],y=cA(zJ,function(a){throw[0,P,zI]},n)[2],z=a(k[66][3],h),A=a(k[66][6],h),B=b(c[i][11],x,z),s=[0,b(S[1],0,zK),0];function
D(h){if(bf[1]){var
k=a(cc(d),h),l=a(f[5],0),m=a(f[3],zL),o=b(f[12],m,l),p=b(f[12],o,k);b(at[6],0,p)}var
q=a0[1],r=[0,a(j[1][6],zM),0,q],s=aa(n),t=g(C[1][13],c[9],0,n);function
u(g){var
f=[0,g],j=[0,ga(0,f,d,ev(d,f,r,h,0,s,B))[3],t],k=b(U[55],f[1],j),l=a(e[17][9],y),m=b(c[i][4],l,k);return[0,f[1],m]}return b(cf[2],1,u)}if(p)var
E=p[1],F=[0,[0,j[1][10][1]]],G=function(a){return ei(d,F,a)},I=b(e[17][15],G,E),J=function(c){var
d=c[1],f=c[2];function
g(e){var
c=a(C[2][1][1],e);return b(j[1][1],c,l)?[0,d,f]:[0,0,[0,c,0]]}var
h=b(e[17][17],g,o);return[0,b(H[25],q,d),h,s]},K=b(e[17][15],J,I),t=a(k[16],K);else{var
u=et([0,d,[0,A]],w,n);if(u)var
L=a(e[19][11],u[1][3]),M=a(H[30][2],L),N=function(a){return iy(0,0,a)},O=b(e[17][15],N,M),Q=function(a){return[0,q,a,s]},R=b(e[17][15],Q,O),v=a(k[16],R);else
var
T=a(j[1][9],l),V=a(f[3],zN),W=b(f[12],V,T),v=b(m[66][5],0,W);var
t=v}return b(k[71][1],t,D)}try{var
D=function(f,e){var
d=f,c=e;for(;;){if(c){var
g=c[2],h=a(C[2][1][1],c[1]);if(b(j[1][1],l,h))return d;var
d=d+1|0,c=g;continue}throw X}}(1,o),E=a(k[16],D),t=E}catch(c){c=G(c);if(c!==X)throw c;var
z=a(j[1][9],l),A=a(f[3],zO),B=b(f[12],A,z),t=b(m[66][5],0,B)}return b(k[71][1],t,y)}return a(k[66][9],d)}aH(1098,[0,kd,ke,kf,gp,gq,kg,kh,zr,gr,kj,kk,kl,kn,cW,gs],ls);function
ko(a){var
c=a[4];function
d(a){return b(S[1],0,[1,a[3]])}var
f=b(e[17][15],d,c);return b(cN[2][88],1,f)}function
kp(b,a){return g(aC[1],zP,b,a)}function
zQ(b){var
a=kp(zS,zR);if(1===a[0])return a[1];throw[0,P,zT]}var
zU=aO[50];function
kq(c,a){function
f(c){function
f(a){return d(a[2])}var
a=b(e[17][15],f,c);if(b(e[17][25],H[3],a))return 0;function
g(a){if(a)if(0!==a[1])return 1;return 0}return b(e[17][26],g,a)?z0:z1}function
d(c){var
a=b(e[17][15],g,c);if(b(e[17][25],H[3],a))return 0;function
d(a){if(a)if(0!==a[1])return 1;return 0}return b(e[17][26],d,a)?zY:zZ}function
g(e){var
a=e[3];switch(a[0]){case
0:var
g=a[2];return b(cF[34],c,a[1])?zV:f(g);case
2:return zW;case
3:var
h=a[2];return b(cF[34],c,a[1])?zX:d(h);default:return 0}}return f(a)}function
kr(m,u,s){var
h=a(A[2],0),f=[0,a(w[17],h)];function
y(n,e){var
g=e[2],i=e[1],j=b(A[47],h,[1,g[1]]),k=j[1],o=j[2];if(m[1]){var
l=dm(o),p=l[1];f[1]=L(w[lY],0,0,w[c2],f[1],l[2]);var
q=[0,a(au[16],k),p],d=a(aO[35],q)}else
var
d=k;var
r=[0,a(c[8],d)];return[0,[0,i,g,d],$([0,[0,i[1]],r,n])]}var
z=g(e[17][21],y,u,s),r=a(e[17][44],z),B=r[1],t=a(e[17][9],r[2]);function
D(o){var
k=o[3],i=o[2],d=o[1],af=o[2],ag=o[1];function
O(c){var
b=a(ak,c),d=b[1],e=a(H[7],b[2]);return[0,a(K[10][16],d),e]}var
Q=b(e[17][15],O,t),n=d[1],l=d[2],r=d[3],y=d[6],R=d[4];if(y){var
z=y[1];if(0===z[0]){var
v=d[5];if(v){var
x=v[1];if(0===x[0])var
q=0;else
if(x[1])var
q=0;else
var
N=function(c){var
e=a(C[1][1][1],c),f=a(K[10][16],e);return 1-b(j[1][1],f,d[1])},B=b(e[17][33],N,t),q=1}else
var
q=0;if(!q)var
B=t;var
p=aa(l),T=p[3],U=p[2],W=[0,b(e[18],p[1],B),U,T],X=i[3],Y=a(c[8],k),D=eP(h,f,d[1],d[6],Y,W,Q,X),u=[0,[0,n,D[2],k,p,D[1]]]}else{var
Z=z[1],F=aa(l),_=i[3],$=[0,[0,n,a(c[8],k)],0],ab=a(c[8],k),G=eP(h,f,d[1],d[6],ab,F,$,_),s=G[2],I=G[1],J=b(K[5],n,z2),ac=function(ah,u,h,ag){var
z=b(j[1][10][7],i[4][6],h[6]),B=h[5],C=b(e[18],i[4][4],h[4]),o=[0,h[1],i[4][2],h[3],C,B,z];ko(o);var
v=h[1];if(1===v[0]){var
p=v[1],x=ar(f,h[1]),D=function(e,d){var
f=b(u,e,d);return a(c[8],f)},q=a(gd(f[1],D),I),y=b(K[5],J,z4),G=function(D,C,B){b(A[52],[0,p],eN[3]);function
h(m,d){var
e=a(aN[34],d[2]),f=[1,a(ao[11],e)],h=a(A[2],0),c=g(aO[31],0,h,f),i=[0,b(S[1],0,[0,c,1,0]),0],j=b(E[17],o[2],z5);b(ci[1],j,i);var
k=[0,b(S[1],0,[0,c,0,0]),0],l=b(E[17],o[2],z6);return b(ci[1],l,k)}b(aJ[8][10],h,s);var
e=a(A[2],0);if(1-m[1])f[1]=a(w[17],e);var
j=[0,n,l,r,r,0,0,d[7]],t=[0,[0,j,[0,p,u,q,o],[0,n,s,b(c[5],f[1],x),F,q]],0],v=i[3],z=[0,[0,a(c[8],k),y,v]];return dG(m[3],e,f[1],z,t)},H=a(A[2],0),t=[0,a(w[17],H)],L=[0,x,V(0,l)],M=a(c[21],L),N=V(0,l),O=[0,a(c[8],k),N],Q=a(c[21],O),R=bi(a(A[2],0),t,r,Q,M),T=b(c[37],R,l),U=i[3],W=i[1],X=function(a){return jU(o,s,Z,W,p,U,q,a)},Y=[0],_=0,$=[0,a(bV[1],G)],aa=[0,function(a){return a}],ab=[0,aB(X)],ac=[0,o[3]],ad=[0,d[7]],ae=a(w[br],t[1]),af=b(c[5],t[1],T);bL(aT[7],y,0,af,ae,0,ad,ac,ab,aa,$,_,Y);return 0}throw[0,P,z3]};gb(0,0,m[1],d[7],z7,f,h,[0,J,l,R],0,I,ac);var
u=0}}else
var
L=aa(l),ad=i[3],ae=a(c[8],k),M=eP(h,f,d[1],d[6],ae,L,0,ad),u=[0,[0,n,M[2],k,L,M[1]]];return[0,ag,af,u]}var
d=b(e[17][15],D,B);if(d){var
i=d[1],l=i[3],n=i[2],k=i[1];if(l){if(!d[2]){var
o=l[1],p=f[1],q=k[6];return q?0===q[1][0]?dG(m[3],h,p,0,[0,[0,k,n,o],0]):0:dG(m[3],h,p,0,[0,[0,k,n,o],0])}}else
if(!d[2])return 0}function
v(b){var
c=b[2],d=b[1];return[0,d,c,a(H[7],b[3])]}var
x=b(e[17][15],v,d);return dG(m[3],h,f[1],0,x)}function
gt(b){var
a=b[5];if(a)if(0!==a[1][0])return 1;return 0}function
ks(M,m,af){function
s(a){return b(e[17][29],a,M)?0:1}try{var
aX=function(a){return 1===a[0]?[0,a[1]]:0},aY=b(e[17][c3],aX,M),y=aY}catch(a){a=G(a);if(a!==X)throw a;var
y=0}var
ag=s(z_),ai=s(z$),N=1-s(Aa);if(m){var
O=m[1],z=O[1],aj=m[2],ak=O[2],al=z[4],am=z[3],an=z[1],D=y?[0,[0,[0,an,[0,[0,1,[0,y[1]]]],am,al],ak],aj]:m,E=N?1-cp[1]:N,q=a(A[2],0),n=a(he[31],0),x=[0,n,ai,ag],d=[0,a(w[17],q)],aq=function(M){var
s=M[1],N=s[2],l=s[1][2],ab=s[4],ac=s[3],ad=o(a0[25],0,0,0,q),O=g(B[65],ad,d,ac)[2],t=O[2],P=O[1],ae=P[2],af=a(a0[16],P[1]);function
ag(a){return b(af,a,0)}var
Q=g(B[65],ag,d,ab),h=b(B[41],d[1],ae),ah=b(B[35],d[1],Q),p=kq(l,D);function
R(b,a){return 0===b?[1,[0,a]]:[0,a]}function
y(b){return[0,a(e[17][1],h)-1|0,0]}if(N){var
T=N[1],U=T[2],z=T[1];if(U){var
u=U[1];try{var
aR=b(r[7],u[2],h)[1],aS=[0,R(z,[0,a(e[17][1],h)-aR|0,[0,u]])],k=Ao,i=aS}catch(c){c=G(c);if(c!==X)throw c;var
ai=a(f[3],Ab),aj=a(j[1][9],u[2]),ak=a(f[3],Ac),al=b(f[12],ak,aj),am=b(f[12],al,ai),V=bR([0,[0,u[1]],Ad,am]),k=V[1],i=V[2]}}else
if(kq(l,[0,M,0]))var
k=Ap,i=[0,R(z,y(0))];else
if(0===z)var
k=p,i=Aq;else
var
k=Ar,i=[0,[0,y(0)]]}else{if(p)if(0===p[1])var
k=p,i=[0,[0,y(0)]],J=1;else
var
J=0;else
var
J=0;if(!J)var
k=p,i=0}var
C=b(c[38],ah,h);o(kt[13],q,w[16],d[1],C);d[1]=a(w[164],d[1]);var
F=b(B[35],d[1],Q);if(E){o(kt[13],q,w[16],d[1],C);var
I=b(K[5],l,Ae),an=[0,[0,dV(0,[0,n],d,0,C)],Af],m=L(bg[3],0,0,I,0,an),ao=b(B[35],d[1],F),ap=b(B[41],d[1],h);if(n)var
W=d[1];else
var
aQ=a(A[2],0),W=a(w[17],aQ);d[1]=W;var
aq=ar(d,[1,m]),as=[0,aq,a8(0,a(e[17][1],ap))],at=a(c[21],as);eb(m,0,Ag);eb(m,0,Ah);eb(m,0,Ai);o(cd[26],1,[1,m],0,[0,t,0]);var
au=0,av=[1,I],aw=S[1],ax=[0,function(a){return b(aw,0,a)}(av),au];b(cN[2][88],1,ax);var
Y=[0,[0,I,m]],x=at,v=ao}else
var
Y=0,x=F,v=F;if(k){var
ay=k[1],Z=b(K[5],l,Aj),az=[0,$([0,[0,a(j[1][6],Ak)],0,x]),h],aA=a(c[9],1),_=b(c[38],aA,az);g(fk,a(A[2],0),d,_);var
aB=b(w[c_],n,d[1]),aC=b(c[5],d[1],_),aD=[0,[0,aF(bg[2],0,0,0,0,[0,aB],0,aC)],Al],aa=L(bg[3],0,0,Z,0,aD),aE=E?[0,[0,[0,a(e[17][1],h)+1|0,0],Am],0]:0,aG=[0,b(e[18],t,aE),0];o(cd[26],1,[1,aa],0,aG);var
aH=0,aI=[1,Z],aJ=S[1],aK=[0,function(a){return b(aJ,0,a)}(aI),aH];b(cN[2][88],1,aK);var
aL=a(e[17][1],h)+1|0,aM=b(c[5],d[1],x),aN=function(a){return a[2]},aO=[1,[0,b(H[16],aN,Y),aM,aa,aL]],aP=ay?[0,[1,aO]]:An;return[0,l,h,x,v,i,aP,t]}return[0,l,h,v,v,i,0,t]},Q=b(e[17][15],aq,D),as=function(a){var
b=a[5],c=b?b[1]:As;return[0,a[1],c]},at=b(e[17][15],as,Q),av=function(a){var
b=a[6];if(b)if(0===b[1][0])return[0,a[1],a[2],a[3],a[4],a[5],[0,[0,at]],a[7]];return a},k=b(e[17][15],av,Q),aw=function(a){return a[2]},R=b(e[17][15],aw,D),h=a(A[2],0),ax=function(a){var
d=b(c[37],a[4],a[2]),e=b(c[37],a[3],a[2]);return[0,a[1],[0,d,e],a[7]]},W=b(e[17][15],ax,k),I=a(e[17][g1],W),Y=I[2],Z=I[1],ay=I[3],az=function(a){return a[1]},aA=b(e[17][15],az,Y),_=aF(a0[3],h,d[1],0,0,Z,aA,ay),aB=function(f){var
b=u(ah),g=f[2],h=v===b?ah[1]:l===b?a(t[2],ah):ah,e=u(ap),i=ar(d,h),j=v===e?ap[1]:l===e?a(t[2],ap):ap,k=[0,0,ar(d,j),i,g];return a(c[20],k)},ab=b(e[17][15],aB,Y),aC=function(b,a){return $([0,[0,b],0,a])},aD=g(e[17][21],aC,Z,ab),aE=a(e[17][9],aD),aG=function(a){return L(cd[20],h,d[1],a[2][1],0,a[3])},aH=b(e[17][15],aG,W),aJ=0,aK=function(d){var
a=b(ku[9],h,_);b(e[17][14],a,af);function
c(d,a,c){var
f=a[6],g=a[1];function
i(a){return im(g,f,h,d,a)}return b(e[17][15],i,c)}return o(e[17][79],c,aH,k,R)},aL=b(ku[14],aK,aJ),p=b(B[41],d[1],aE),aM=function(c,r){var
g=b(B[41],d[1],c[2]);if(ij(c[6])){var
i=c[5];if(i){var
k=i[1];if(0===k[0])var
f=0;else
if(k[1])var
f=0;else
var
o=function(d){var
e=a(C[1][1][1],d),f=a(K[10][16],e);return 1-b(j[1][1],f,c[1])},q=b(e[17][33],o,p),l=aa(b(e[18],g,q)),f=1}else
var
f=0;if(!f)var
l=aa(b(e[18],g,p));var
m=l}else
var
m=aa(g);b(B[35],d[1],c[4]);var
n=b(B[35],d[1],c[3]);return ev(h,d,[0,c[1],E,_],r,[0,[1,c[1]],0],m,n)},aO=g(e[17][21],aM,k,aL),ac=j[61],ad=u(ap),aP=ac[2],aQ=ac[1],aR=v===ad?ap[1]:l===ad?a(t[2],ap):ap,aS=a(au[8],aR),aT=a(e[17][5],k)[1],aU=a(j[1][8],aT),aV=[0,aQ,b(j[18][8],aS,aP)];o(aI[17],0,aU,aV,1);var
J=bK(a(e[17][1],R),0),ae=[0,0],aW=function(f,y){var
m=f[6];if(m){var
q=m[1];if(0===q[0])var
l=0;else
var
s=[0,q[1]],l=1}else
var
l=0;if(!l)var
s=0;var
t=f[5];if(t){var
u=t[1];if(0===u[0])var
k=0;else
if(u[1])var
k=0;else
var
D=function(c){var
d=a(C[1][1][1],c),e=a(K[10][16],d);return 1-b(j[1][1],e,f[1])},v=b(e[17][33],D,p),k=1}else
var
k=0;if(!k)var
v=p;var
_=ae[1];function
z(bt,$,q,bu){ko(q);var
aa=0,ac=q[5];function
ad(e,d){var
f=a(aN[34],e),b=[0,[1,a(ao[11],f)]],c=cT(q);return[0,[0,aI[4],c,1,0,b],d]}var
ae=[0,g(j[1][10][15],ad,ac,aa)],af=[0,gf(q),0];g(aI[22],0,af,ae);var
S=q[1];if(1===S[0]){var
bj=S[1];d[1]=a(w[18],bu);var
bk=function(e,d){var
f=b($,e,d);return a(c[8],f)},bl=a(gd(d[1],bk),bt),E=d[1],ag=f[7],ah=f[6],ai=f[5],aj=b(B[35],E,f[4]),ak=b(B[35],E,f[3]),al=b(B[41],E,f[2]),bn=[0,[0,[0,f[1],al,ak,aj,ai,ah,ag],[0,bj,$,bl,q]]];T(J,_)[_+1]=bn;var
bo=function(b){return 1-a(H[3],b)},W=b(bm[34],bo,J);if(W){var
bp=a(B[35],d[1]),bq=b(e[17][15],bp,ab),br=function(b){return a(H[7],b)},l=b(e[19][52],br,J);if(l)if(l[2])var
F=0;else
var
X=l,F=1;else
var
F=0;if(!F){var
am=function(a){return 1-gt(a[1])},m=b(e[17][33],am,l),an=function(f){var
b=f[1],c=b[5];if(c){var
d=c[1];if(0===d[0])return d[1][1]}return a(e[17][1],b[2])-1|0},ap=b(e[19][53],an,m),aq=a(A[2],0),s=[0,a(w[17],aq)],as=a(e[17][1],m),t=a(e[17][1],l)-as|0,z=a(e[17][1],m),p=0,h=0,n=l;a:for(;;){if(n){var
C=n[2],G=n[1],I=G[2],k=G[1],K=k[5];if(K){var
L=K[1];if(0!==L[0]){var
M=L[1];if(M){var
aP=M[1][1],u=a(e[17][1],k[2]),at=a8(u+(t-1|0)|0,a(e[17][1],m)),au=[0,a(c[22],I[1]),at],av=a(c[21],au),aw=b(c[i][1],1,av),az=bK(1,[0,k[1]]),aA=bK(1,b(c[37],k[3],k[2])),aB=b(r[9],u+1|0,p),aE=[0,a(c[9],u+1|0),0],y=b(e[18],aB,aE),v=0,ax=(t-1|0)-p|0,ay=[0,bK(1,aP),0];for(;;){if(v===ax){var
aF=a(c[34],[0,aw,y]),aG=[0,aF,V(0,k[2])],aH=a(c[21],aG),aJ=b(c[38],aH,k[2]),aK=function(a){return[0,0,c[14]]},aL=b(e[17][56],t-1|0,aK),aM=a(c[31],[0,ay,[0,az,aA,bK(1,aJ)]]),p=p+1|0,h=[0,[0,1,b(c[38],aM,aL)],h],n=C;continue a}var
aC=[0,a(c[9],(u+t|0)-v|0),y],aD=[0,a(c[34],aC),0],y=b(e[18],y,aD),v=v+1|0;continue}}var
aQ=a8(0,a(e[17][1],m)),aR=[0,a(c[22],I[1]),aQ],p=p+1|0,h=[0,[0,1,a(c[21],aR)],h],n=C;continue}}var
aO=[0,[0,0,a(c[9],z)],h],z=z-1|0,h=aO,n=C;continue}var
aS=a(e[17][9],h),aT=function(a){return a[1]},N=b(e[17][35],aT,aS),aU=N[2],aV=N[1],aW=0,aX=function(d,b){return[0,a(c[34],[0,d[2],b]),b]},aY=g(e[17][19],aX,aV,aW),aZ=0,a0=function(b,d){var
f=[0,d,a(e[17][9],b)];return[0,a(c[34],f),b]},a1=g(e[17][18],a0,aZ,aY),a2=s[1],a3=a(A[2],0),a4=b(U[17],a3,a2),O=b(e[17][17],a4,a1),a5=function(a){return a[2]},a6=b(e[17][15],a5,aU),a7=function(f){var
d=f[1],g=[0,d[1]],h=b(B[14],s,[1,f[2][1]]),j=b(c[37],d[3],d[2]),k=a(e[19][12],O),l=a(e[19][12],a6),m=[0,h,b(e[19][5],l,k)],n=a(c[21],m),o=V(0,d[2]),p=a(e[17][1],d[2]),q=[0,b(c[i][1],p,n),o],r=a(c[21],q);return[0,g,j,b(c[38],r,d[2])]},a9=b(e[17][15],a7,m),D=a(e[17][g1],a9),a_=D[2],a$=D[1],ba=a(e[19][12],D[3]),bb=a(e[19][12],a_),bc=[0,a(e[19][12],a$),bb,ba],bd=function(h,f){var
e=f[2],d=f[1];if(gt(d))return[0,d,e];var
i=a(c[31],[0,[0,ap,h],bc]),j=b(c[37],d[3],d[2]),g=bw(d[1],i,[0,j],x[1],s[1],z8);o(cd[26],1,[1,g],0,[0,d[7],0]);return[0,d,[0,g,e[2],e[3],e[4]]]},be=b(e[17][16],bd,l),bf=function(a){return gt(a[1])},Q=b(e[17][35],bf,be),R=Q[2],bg=Q[1],bh=function(f,h){var
d=f[2],a=f[1],j=b(c[37],a[3],a[2]);function
k(a){return ar(s,[1,a[2][1]])}var
l=b(e[17][17],k,R),m=b(c[i][4],l,h),g=bw(a[1],m,[0,j],x[1],s[1],z9);o(cd[26],1,[1,g],0,[0,a[7],0]);return[0,a,[0,g,d[2],d[3],d[4]]]},bi=g(e[17][21],bh,bg,O),X=b(e[18],R,bi);break}}var
bs=x[2],Y=bs||x[3];if(Y)return kr(x,bq,X);var
Z=Y}else
var
Z=W;return Z}throw[0,P,Au]}gb(f[6],v,n,f[7],At,d,h,[0,f[1],f[2],f[4]],s,y,z);ae[1]++;return 0};return g(F[20],aW,k,aO)}throw[0,P,Av]}function
kv(d,a,c){function
f(c){var
a=c[1][1],d=b(S[1],[0,a[1]],a[2]);return g(dn[14],d,0,Aw)}b(e[17][14],f,a);return ks(d,a,c)}function
kw(A,x,d){var
B=a(y[7],d);function
i(e,h){var
c=a(R[26],e);switch(c[0]){case
6:var
k=c[1];if(k){var
p=c[3],q=k[1],r=a(y[8],d),l=dY(j[1][10][1],q,r),t=a(R[2],l),f=i(b(a1[14],t,p),[0,l]),u=f[3],v=f[2],w=f[1],x=z(s[16]);return[0,b(m[5],x,w),v,u]}break;case
8:var
n=c[1];if(n){var
o=n[1],A=c[4],B=c[2];if(an.caml_string_equal(a(j[1][8],o),Ax))return[0,m[1],h,e];var
C=a(y[8],d),D=[0,dY(j[1][10][1],o,C)],g=i(b(a1[14],B,A),D),E=g[3],F=g[2],G=g[1],H=z(s[16]);return[0,b(m[5],H,G),F,E]}break}return[0,m[1],h,e]}var
C=a(y[2],d),k=i(b(c[5],C,B),0),o=k[2],D=k[3],E=k[1];if(o)var
F=o[1],p=function(a){return z(b(s[81],a,[1,F]))};else
var
p=function(a){return m[1]};var
G=a(c[8],D),H=a(y[2],d),g=b(c[3],H,G);if(8===g[0]){var
u=g[1];if(u){var
X=g[4],Y=g[2],Z=u[1],_=a(y[2],d),h=b(c[3],_,X);if(8===h[0]){var
w=h[1];if(w)var
$=h[4],aa=h[2],ab=w[1],ac=a(y[2],d),ad=dX(b(c[5],ac,aa)),ae=a(y[2],d),v=[0,Z,ab,dX(b(c[5],ae,Y)),ad,$],n=1;else
var
n=0}else
var
n=0;if(!n)var
v=a7(AA);var
f=v,l=1}else
var
l=0}else
var
l=0;if(!l)var
f=a7(Ay);var
I=f[5],J=f[4],K=f[3],M=f[2],N=f[1];function
q(g,f){if(0===g)return[0,0,f];var
j=a(y[2],d),e=b(c[3],j,f);if(8===e[0]){var
h=e[1];if(h){var
k=e[3],l=e[2],m=h[1],i=q(g-1|0,e[4]);return[0,[0,[0,m,l,k],i[1]],i[2]]}}return a7(Az)}var
r=q(J,I)[1],t=[0,N,[0,M,b(e[17][15],jY,r)]],O=z(a(s[75],t)),P=z(a(s[25],t)),Q=b(m[5],P,O),S=z(s[16]),T=b(m[26],K+1|0,S);function
U(a){var
c=a[1],d=a[3],e=a[2],f=p(c),g=b(m[5],f,x),h=z(L(s[bN],0,[0,c],e,[0,d],cw));return b(m[5],h,g)}var
V=b(e[17][15],U,r),W=[0,E,[0,Q,[0,T,[0,b(m[11],A,V),0]]]];return b(m[7],W,d)}aH(1101,[0,kp,zQ,zU,ks,kr,kv,kw,function(c,f,e,d){var
a=b(aA[79],c,e),h=g(d_,function(a,d){var
e=aZ(d),h=g(r[99],c,f,d);return b(j[1][10][3],e,a)?b(j[1][10][7],h,a):a},a,d);return[0,a,b(j[1][10][9],h,a)]}],N);function
kx(i,h,g){function
d(d){var
l=a(k[66][6],d),e=b(c[82],l,g),f=e[1],m=e[2],n=b(y[42][21],d,f),o=a(aK[12],m),p=[0,a(c[9],1),o],q=a(c[21],p),r=[0,[0,a(j[1][6],AB)],n,q],t=a(c[19],r),u=L(s[bN],0,[0,h],t,0,dv[5]),v=L(s[bN],0,[0,i],f,0,dv[5]);return b(k[18],v,u)}return a(k[66][10],d)}function
ky(c){if(1===c[0]){var
d=a(j[17][9],c[1]),e=a(j[6][5],d);return b(AE[7],[0,AD,[0,e,0]],dv[6])}throw[0,P,AC]}function
kz(n){function
d(h){var
x=a(k[66][5],h),e=a(k[66][6],h),z=a(k[66][3],h),A=ax[2];function
B(a){return g(A,0,0,a)}var
C=g(y[42][1],B,h,n),p=j[61],q=[0,e],f=x,l=z,r=C;for(;;){var
i=b(c[3],e,l),d=b(c[3],e,r);if(6===i[0]){var
w=i[2],P=i[3],Q=i[1];switch(d[0]){case
6:var
R=d[3];if(L(eI[4],f,[0,p],q,w,d[2])){var
S=$([0,Q,0,w]),f=b(c[bu],S,f),l=P,r=R;continue}return a7(AH);case
9:var
m=0;break;default:var
m=1}}else
var
m=0;if(!m)if(9===d[0]){var
s=d[1],D=d[2];if(b(c[47],e,s)){var
E=b(c[75],e,s),F=q[1],t=o(ic,f,F,E,a(aK[11],D)),u=t[2],G=t[1],H=u[2],I=function(a){return 0},J=b(bm[52],I,H),v=c0(eI[10],p,f,G,u,J,l),K=v[1];if(v[2]){var
M=function(a){return[0,a,n]},N=b(cf[2],0,M),O=a(k[64][1],K);return b(k[71][2],O,N)}return a7(AG)}}return a7(AF)}}return a(k[66][9],d)}aH(1103,[0,kx,ky,kz],lc);a(aX[10],am);var
eQ=k[70][1],AI=0;function
AJ(c,b,a,d){return kx(c,b,a)}var
AL=a(j[1][7],AK),AM=[0,[5,a(n[16],D[13])],AL],AN=[1,b(J[11],0,AM),0],AP=a(j[1][7],AO),AQ=[0,[5,a(n[16],D[8])],AP],AR=[1,b(J[11],0,AQ),AN],AT=a(j[1][7],AS),AU=[0,[5,a(n[16],D[8])],AT],AW=[0,[0,[0,AV,[1,b(J[11],0,AU),AR]],AJ],AI];o(M[10][8],am,AX,0,AW);var
AY=0;function
AZ(a,b){return ky(a)}var
A1=a(j[1][7],A0),A2=[0,[5,a(n[16],D[23])],A1],A4=[0,[0,[0,A3,[1,b(J[11],0,A2),0]],AZ],AY];o(M[10][8],am,A5,0,A4);var
A6=0;function
A7(c,a,d){return b(cP[5],c,a)}var
A9=a(j[1][7],A8),A_=[0,[5,a(n[16],D[8])],A9],A$=[1,b(J[11],0,A_),0],Bb=a(j[1][7],Ba),Bc=[0,[5,a(n[16],D[9])],Bb],Be=[0,[0,[0,Bd,[1,b(J[11],0,Bc),A$]],A7],A6];o(M[10][8],am,Bf,0,Be);var
Bg=0;function
Bh(b,c){return a(cP[4],b)}var
Bj=a(j[1][7],Bi),Bk=[0,[5,a(n[16],D[9])],Bj],Bn=[0,[0,[0,Bm,[0,Bl,[1,b(J[11],0,Bk),0]]],Bh],Bg];o(M[10][8],am,Bo,0,Bn);var
Bp=0,Br=[0,[0,Bq,function(a){return cP[2]}],Bp];function
Bs(b,c){return a(cP[1],b)}var
Bu=a(j[1][7],Bt),Bv=[0,[5,a(n[16],D[9])],Bu],Bx=[0,[0,[0,Bw,[1,b(J[11],0,Bv),0]],Bs],Br];o(M[10][8],am,By,0,Bx);var
Bz=0;function
BA(a,b){return jd(a)}var
BC=a(j[1][7],BB),BD=[0,[5,a(n[16],D[8])],BC],BF=[0,[0,[0,BE,[1,b(J[11],0,BD),0]],BA],Bz];o(M[10][8],am,BG,0,BF);var
BH=0;function
BI(c,a,d){return b(cP[3],c,a)}var
BK=a(j[1][7],BJ),BL=[0,[5,a(n[16],D[8])],BK],BM=[1,b(J[11],0,BL),0],BO=a(j[1][7],BN),BP=[0,[5,a(n[16],D[13])],BO],BR=[0,[0,[0,BQ,[1,b(J[11],0,BP),BM]],BI],BH];o(M[10][8],am,BS,0,BR);var
BT=0;function
BU(a,e){var
c=0;function
d(b){return gr(c,a,b)}return b(k[70][1],0,d)}var
BW=a(j[1][7],BV),BX=[0,[5,a(n[16],D[13])],BW],B0=[0,[0,[0,BZ,[0,BY,[1,b(J[11],0,BX),0]]],BU],BT];o(M[10][8],am,B1,0,B0);var
B2=0;function
B3(a,d){function
c(b){return gr(B4,a,b)}return b(k[70][1],0,c)}var
B6=a(j[1][7],B5),B7=[0,[5,a(n[16],D[13])],B6],B$=[0,[0,[0,B_,[0,B9,[0,B8,[1,b(J[11],0,B7),0]]]],B3],B2];o(M[10][8],am,Ca,0,B$);var
Cb=0;function
Cc(a,e){var
c=0;function
d(b){return kl(c,a,b)}return b(k[70][1],0,d)}var
Ce=a(j[1][7],Cd),Cf=[0,[5,a(n[16],D[13])],Ce],Ch=[0,[0,[0,Cg,[1,b(J[11],0,Cf),0]],Cc],Cb];o(M[10][8],am,Ci,0,Ch);var
Cj=0,Cl=[0,[0,0,function(c){if(c){var
d=c[2];if(d){var
e=d[2];if(e){var
f=e[2];if(f){var
g=f[2];if(g){var
h=g[2];if(h){var
i=h[2];if(i){var
j=i[2];if(j){var
k=j[2];if(k){var
l=k[2];if(l)if(!l[2]){var
m=l[1],o=k[1],p=j[1],q=i[1],r=h[1],s=g[1],t=f[1],u=e[1],v=d[1],w=c[1],x=a(n[4],D[12]),y=b(n[8],x,w),z=a(n[4],D[24]),A=b(n[8],z,v),B=a(n[4],D[24]),C=b(n[8],B,u),F=a(n[4],D[24]),G=b(n[8],F,t),H=a(n[4],D[24]),I=b(n[8],H,s),J=a(n[4],D[24]),K=b(n[8],J,r),L=a(n[4],D[24]),M=b(n[8],L,q),N=a(n[4],D[24]),O=b(n[8],N,p),P=a(n[4],D[24]),Q=b(n[8],P,o),R=a(n[4],D[24]),S=b(n[8],R,m);return function(l,c){function
b(b){var
c=a(ao[18],b);return a(hg[4],c)}var
d=b(S),e=b(Q),f=b(O),g=b(M),h=b(K),i=b(I),j=b(G),k=b(C);hh([0,b(A),k,j,i,y,h,g,f,e,d]);return c}}}}}}}}}}}return a(E[3],Ck)}],Cj];function
Cm(b,a){return g(gu[2],a[1],[0,Cn,b],a[2])}b(F[89],Cm,Cl);var
Co=0,Cq=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d){var
e=d[2];if(e){var
f=e[2];if(f){var
g=f[2];if(g){var
h=g[2];if(h){var
i=h[2];if(i){var
j=i[2];if(j){var
k=j[2];if(k)if(!k[2])return function(a){return cX[4]}}}}}}}}}}return a(E[3],Cp)},Co];function
Cr(c,a){return b(cX[3],[0,Cs,c],a)}b(F[89],Cr,Cq);var
Ct=[6,a(h[12],D[24])],Cu=[0,[0,a(n[4],D[24])],Ct],Cv=[0,[1,b(J[11],0,Cu)],0],Cw=[6,a(h[12],D[24])],Cx=[0,[0,a(n[4],D[24])],Cw],Cy=[0,[1,b(J[11],0,Cx)],Cv],Cz=[6,a(h[12],D[24])],CA=[0,[0,a(n[4],D[24])],Cz],CB=[0,[1,b(J[11],0,CA)],Cy],CC=[6,a(h[12],D[24])],CD=[0,[0,a(n[4],D[24])],CC],CE=[0,[1,b(J[11],0,CD)],CB],CF=[6,a(h[12],D[24])],CG=[0,[0,a(n[4],D[24])],CF],CH=[0,[1,b(J[11],0,CG)],CE],CI=[6,a(h[12],D[24])],CJ=[0,[0,a(n[4],D[24])],CI],CK=[0,[1,b(J[11],0,CJ)],CH],CL=[6,a(h[12],D[24])],CM=[0,[0,a(n[4],D[24])],CL],CN=[0,[1,b(J[11],0,CM)],CK],CO=[6,a(h[12],D[24])],CP=[0,[0,a(n[4],D[24])],CO],CQ=[0,[1,b(J[11],0,CP)],CN],CR=[6,a(h[12],D[24])],CS=[0,[0,a(n[4],D[24])],CR],CT=[0,[1,b(J[11],0,CS)],CQ],CU=[6,a(h[12],D[12])],CV=[0,[0,a(n[4],D[12])],CU],CY=[0,[0,CX,[0,CW,[0,[1,b(J[11],0,CV)],CT]]],0];function
CZ(b,a){return g(gv[1],[0,C0,b],0,a)}b(F[89],CZ,CY);var
C1=0;function
C2(c,e){function
d(b){if(kg(b,c))return a(cz[9],b);var
d=a(f[3],C3);return g(cz[41],0,d,b)}return b(k[70][1],0,d)}var
C5=a(j[1][7],C4),C6=[0,[5,a(n[16],D[9])],C5],C8=[0,[0,[0,C7,[1,b(J[11],0,C6),0]],C2],C1];o(M[10][8],am,C9,0,C8);var
C_=0;function
C$(d,c,a){var
e=z(b(M[13][24],a,c)),f=z(b(M[13][24],a,d));return b(eQ,0,function(a){return kw(f,e,a)})}var
Db=a(j[1][7],Da),Dc=[0,[5,a(n[16],M[2][1])],Db],Dd=[1,b(J[11],0,Dc),0],Df=a(j[1][7],De),Dg=[0,[5,a(n[16],M[2][1])],Df],Di=[0,[0,[0,Dh,[1,b(J[11],0,Dg),Dd]],C$],C_];o(M[10][8],am,Dj,0,Di);var
Dk=0;function
Dl(e,d,f){return b(eQ,0,gg(d,a(hC,b(p[17],c[bO][1],e))))}var
Dn=a(j[1][7],Dm),Do=[0,[5,a(n[16],D[25])],Dn],Dp=[1,b(J[11],0,Do),0],Dr=a(j[1][7],Dq),Ds=[0,[2,[5,a(n[16],D[13])]],Dr],Du=[0,[0,[0,Dt,[1,b(J[11],0,Ds),Dp]],Dl],Dk];function
Dv(c,a,d){return b(eQ,0,gg(a,c))}var
Dx=a(j[1][7],Dw),Dy=[0,[5,a(n[16],D[25])],Dx],Dz=[1,b(J[11],0,Dy),0],DB=a(j[1][7],DA),DC=[0,[0,[5,a(n[16],D[22])]],DB],DE=[0,[0,[0,DD,[1,b(J[11],0,DC),Dz]],Dv],Du];o(M[10][8],am,DF,0,DE);var
bI=a(n[2],DG);function
DH(b,a){return[0,b,a]}b(a$[9],bI,DH);function
DI(b,a){return a}b(a$[10],bI,DI);function
DJ(g,c){var
d=a(n[6],bI),e=a(Y[3],d),f=b(Y[1][8],e,c);return a(b1[1],f)}b(Y[7],bI,DJ);b(Y[4],bI,0);var
DK=a(n[4],bI),eR=g(h[13],h[9],DL,DK),DM=0,DN=0;function
DO(b,a){return DP}var
DR=[0,[0,[0,0,[0,a(bo[10],DQ)]],DO],DN];function
DS(b,a){return DT}var
DV=[0,[0,[0,0,[0,a(bo[10],DU)]],DS],DR];function
DW(b,c,a){return[1,[0,[0,a,b]]]}var
DX=[6,h[14][2]],DZ=[0,[0,[0,[0,0,[0,a(bo[10],DY)]],DX],DW],DV];function
D0(b,a){return D1}var
D3=[0,[0,[0,0,[0,a(bo[10],D2)]],D0],DZ];function
D4(b,a){return D5}var
D7=[0,[0,[0,0,[0,a(bo[10],D6)]],D4],D3];function
D8(b,a){return D9}var
D$=[0,[0,[0,0,[0,a(bo[10],D_)]],D8],D7];function
Ea(b,a){return Eb}var
Ed=[0,[0,[0,0,[0,a(bo[10],Ec)]],Ea],D$];function
Ee(b,a){return Ef}var
Eh=[0,0,[0,[0,0,0,[0,[0,[0,0,[0,a(bo[10],Eg)]],Ee],Ed]],DM]];g(h[22],eR,0,Eh);o(M[5][1],bI,ef,ef,ef);var
Ei=[0,eR,0];function
Ej(c){var
d=c[2],e=a(n[4],bI);return[0,b(n[7],e,d)]}g(M[10][5],Ek,Ej,Ei);var
ba=a(n[2],El);function
Em(b,a){return[0,b,a]}b(a$[9],ba,Em);function
En(b,a){return a}b(a$[10],ba,En);function
Eo(g,c){var
d=a(n[6],ba),e=a(Y[3],d),f=b(Y[1][8],e,c);return a(b1[1],f)}b(Y[7],ba,Eo);b(Y[4],ba,0);var
Ep=a(n[4],ba),gw=g(h[13],h[9],Eq,Ep),Er=0,Es=0;function
Et(d,a,c,b){return a}var
Ev=[0,a(bo[10],Eu)],Ex=[0,[0,[0,[0,[0,0,[0,a(bo[10],Ew)]],[1,[6,eR]]],Ev],Et],Es],Ey=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],Ex]],Er]];g(h[22],gw,0,Ey);o(M[5][1],ba,eg,eg,eg);var
Ez=[0,gw,0];function
EA(c){var
d=c[2],e=a(n[4],ba);return[0,b(n[7],e,d)]}g(M[10][5],EB,EA,Ez);function
eS(e,d,c,b){return a(j[1][9],b[2])}var
bJ=a(n[2],EC);function
ED(b,a){return[0,b,a]}b(a$[9],bJ,ED);function
EE(b,a){return a}b(a$[10],bJ,EE);function
EF(g,c){var
d=a(n[6],bJ),e=a(Y[3],d),f=b(Y[1][8],e,c);return a(b1[1],f)}b(Y[7],bJ,EF);b(Y[4],bJ,0);var
EG=a(n[4],bJ),eT=g(h[13],h[9],EH,EG),EI=0,EJ=0;function
EK(b,a){return[0,a,b]}g(h[22],eT,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,h[14][2]]],EK],EJ]],EI]]);o(M[5][1],bJ,eS,eS,eS);var
EL=[0,eT,0];function
EM(c){var
d=c[2],e=a(n[4],bJ);return[0,b(n[7],e,d)]}g(M[10][5],EN,EM,EL);var
EO=0,EP=0;function
kA(e,d,c,b){return a(f[7],0)}function
kB(e,d,c,b){return a(f[7],0)}function
kC(e,d,c,b){return a(f[7],0)}var
eU=a(n[3],EQ),ER=a(n[4],eU),gx=g(h[13],h[8],ES,ER),ET=b(Y[4],eU,0);o(M[5][1],eU,kA,kB,kC);var
eV=a(n[3],EU),EV=b(Y[4],eV,0);function
kD(e,d,c,b){return a(f[7],0)}function
kE(e,d,c,b){return a(f[7],0)}function
kF(e,d,c,b){return a(f[7],0)}var
EW=a(n[4],eV),dH=g(h[13],h[10],EX,EW);o(M[5][1],eV,kD,kE,kF);var
eW=a(n[3],EY),EZ=b(Y[4],eW,0);function
kG(e,d,c,b){return a(f[7],0)}function
kH(e,d,c,b){return a(f[7],0)}function
kI(e,d,c,b){return a(f[7],0)}var
E0=a(n[4],eW),dI=g(h[13],h[9],E1,E0);o(M[5][1],eW,kG,kH,kI);var
cj=a(n[3],E2),E3=b(Y[4],cj,0);function
kJ(e,d,c,b){return a(f[7],0)}function
kK(e,d,c,b){return a(f[7],0)}function
kL(e,d,c,b){return a(f[7],0)}var
E4=a(n[4],cj),kM=g(h[13],h[10],E5,E4);o(M[5][1],cj,kJ,kK,kL);function
E6(b,a){return a}function
kN(e,d){var
c=a(n[2],d);b(Y[4],c,e);return c}var
E8=a(n[6],D[4]),eX=kN([0,a(Y[3],E8)],E7);function
kO(b,a){return[0,b,a]}function
kP(b,a){return a}function
kQ(c,b){return a(b1[1],b)}function
kR(c,d){function
e(f,e){function
g(d){var
e=a(n[6],c),f=a(Y[3],e),g=b(Y[1][8],f,d);return a(b1[1],g)}var
h=b(d,f,e);return b(b1[2],h,g)}return b(Y[7],c,e)}function
kS(a){b(a$[9],a,kO);b(a$[10],a,kP);return kR(a,kQ)}kS(eX);var
E9=a(n[4],eX),kT=g(h[13],h[9],E_,E9),a2=h[1][4][1],cY=a(a2,E$),kU=a(a2,Fa),kV=a(a2,Fb),dJ=a(a2,Fc),kW=a(a2,Fd),eY=a(a2,Fe),kX=a(a2,Ff),gy=a(a2,Fg),kY=a(a2,Fh),gz=a(a2,Fi),kZ=a(a2,Fj),gA=a(a2,Fk),eZ=a(a2,Fl),Fm=0,Fn=0;function
Fo(a,b){return a}var
Fq=a(h[1][16],Fp),Fr=[0,b(h[1][20],h[1][19],Fq),Fo],Fs=[0,[0,0,0,[0,a(h[1][22],Fr),Fn]],Fm];g(h[1][25],kT,0,Fs);var
Ft=0,Fu=0;function
Fv(a,b){return a}var
Fw=a(h[1][6],h[15][16]),Fx=[0,b(h[1][20],h[1][19],Fw),Fv],Fy=[0,[0,0,0,[0,a(h[1][22],Fx),Fu]],Ft];g(h[1][25],gx,0,Fy);var
Fz=0,FA=0;function
FB(a,b){return a}var
FD=a(h[1][16],FC),FE=a(h[1][6],kU),FF=g(h[1][11],FE,FD,0),FG=[0,b(h[1][20],h[1][19],FF),FB],FH=[0,[0,0,0,[0,a(h[1][22],FG),FA]],Fz];g(h[1][25],dH,0,FH);var
FI=0,FJ=0;function
FK(d,a,c,b){return a}var
FM=a(h[1][16],FL),FO=a(h[1][16],FN),FP=a(h[1][6],eY),FQ=g(h[1][9],FP,FO,0),FS=a(h[1][16],FR),FT=b(h[1][20],h[1][19],FS),FU=b(h[1][20],FT,FQ),FV=[0,b(h[1][20],FU,FM),FK],FW=[0,[0,0,0,[0,a(h[1][22],FV),FJ]],FI];g(h[1][25],dI,0,FW);var
FX=0,FY=0;function
FZ(c,b){return[0,a(h[29],b),c]}var
F0=a(h[1][6],h[15][6]),F1=[0,b(h[1][20],h[1][19],F0),FZ],F2=[0,[0,0,0,[0,a(h[1][22],F1),FY]],FX];g(h[1][25],cY,0,F2);var
F3=0,F4=0;function
F5(c,b,a,d){return[0,[0,a],[0,b],c]}var
F6=a(h[1][6],gA),F7=a(h[1][6],kV),F8=a(h[1][10],F7),F9=a(h[1][6],cY),F_=b(h[1][20],h[1][19],F9),F$=b(h[1][20],F_,F8),Ga=[0,b(h[1][20],F$,F6),F5],Gb=[0,a(h[1][22],Ga),F4];function
Gc(b,a,d,c){return[0,0,[1,a],b]}var
Gd=a(h[1][6],gA),Gf=a(h[1][16],Ge),Gg=a(h[1][6],eY),Gh=g(h[1][11],Gg,Gf,0),Gj=a(h[1][16],Gi),Gk=b(h[1][20],h[1][19],Gj),Gl=b(h[1][20],Gk,Gh),Gm=[0,b(h[1][20],Gl,Gd),Gc],Gn=[0,[0,0,0,[0,a(h[1][22],Gm),Gb]],F3];g(h[1][25],kU,0,Gn);var
Go=0,Gp=0;function
Gq(f,b,e,a,d,c){return[0,[0,a],b]}var
Gs=a(h[1][16],Gr),Gt=a(h[1][6],dJ),Gv=a(h[1][16],Gu),Gw=a(h[1][6],cY),Gy=a(h[1][16],Gx),Gz=b(h[1][20],h[1][19],Gy),GA=b(h[1][20],Gz,Gw),GB=b(h[1][20],GA,Gv),GC=b(h[1][20],GB,Gt),GD=[0,b(h[1][20],GC,Gs),Gq],GE=[0,a(h[1][22],GD),Gp];function
GF(a,b){return[0,0,a]}var
GG=a(h[1][6],dJ),GH=[0,b(h[1][20],h[1][19],GG),GF],GI=[0,[0,0,0,[0,a(h[1][22],GH),GE]],Go];g(h[1][25],kV,0,GI);var
GJ=0,GK=0;function
GL(c,b){var
d=[0,[0,a(h[29],b),c],0];return[0,a(h[29],b),d]}var
GM=a(h[1][6],h[14][19]),GN=[0,b(h[1][20],h[1][19],GM),GL],GO=[0,a(h[1][22],GN),GK];function
GP(c,b){return[0,a(h[29],b),0]}var
GR=a(h[1][16],GQ),GS=[0,b(h[1][20],h[1][19],GR),GP],GT=[0,a(h[1][22],GS),GO];function
GU(d,a,c,b){return a}var
GW=a(h[1][16],GV),GX=a(h[1][6],eY),GZ=a(h[1][16],GY),G0=b(h[1][20],h[1][19],GZ),G1=b(h[1][20],G0,GX),G2=[0,b(h[1][20],G1,GW),GU],G3=[0,a(h[1][22],G2),GT];function
G4(e,c,d,b){return[0,a(h[29],b),[1,c]]}var
G6=a(h[1][16],G5),G7=a(h[1][6],h[15][3]),G9=a(h[1][16],G8),G_=b(h[1][20],h[1][19],G9),G$=b(h[1][20],G_,G7),Ha=[0,b(h[1][20],G$,G6),G4],Hb=[0,a(h[1][22],Ha),G3];function
Hc(c,b){return[0,a(h[29],b),[2,c]]}var
He=b(h[1][7],h[15][11],Hd),Hf=[0,b(h[1][20],h[1][19],He),Hc],Hg=[0,[0,0,0,[0,a(h[1][22],Hf),Hb]],GJ];g(h[1][25],dJ,0,Hg);var
Hh=0,Hi=0;function
Hj(c,b){return[0,a(h[29],b),c]}var
Hk=a(h[1][6],h[14][19]),Hl=[0,b(h[1][20],h[1][19],Hk),Hj],Hm=[0,[0,0,0,[0,a(h[1][22],Hl),Hi]],Hh];g(h[1][25],kW,0,Hm);var
Hn=0,Ho=0;function
Hp(d,c,b){return[0,a(h[29],b),[0,c,d]]}var
Hq=a(h[1][6],dJ),Hr=a(h[1][8],Hq),Hs=a(h[1][6],kW),Ht=b(h[1][20],h[1][19],Hs),Hu=[0,b(h[1][20],Ht,Hr),Hp],Hv=[0,a(h[1][22],Hu),Ho];function
Hw(a,b){return a}var
Hx=a(h[1][6],dJ),Hy=[0,b(h[1][20],h[1][19],Hx),Hw],Hz=[0,[0,0,0,[0,a(h[1][22],Hy),Hv]],Hn];g(h[1][25],eY,0,Hz);var
HA=0,HB=0;function
HC(g,f){function
c(a){return a}var
b=g;for(;;){if(b){var
d=b[2],e=b[1];if(d){var
c=function(c,d){return function(b){return c([3,d,[0,[0,0,[1,[0,[0,a(h[29],f),0],0]],b],0]])}}(c,e),b=d;continue}return function(a){return c([3,e,a])}}throw[0,P,HD]}}var
HF=a(h[1][16],HE),HG=a(h[1][6],h[15][3]),HH=g(h[1][11],HG,HF,0),HI=[0,b(h[1][20],h[1][19],HH),HC],HJ=[0,[0,0,0,[0,a(h[1][22],HI),HB]],HA];g(h[1][25],kX,0,HJ);var
HK=0,HL=0;function
HM(e,a,d,c,b){return[0,a]}var
HO=a(h[1][16],HN),HP=a(h[1][6],cY),HR=a(h[1][16],HQ),HT=a(h[1][16],HS),HU=b(h[1][20],h[1][19],HT),HV=b(h[1][20],HU,HR),HW=b(h[1][20],HV,HP),HX=[0,b(h[1][20],HW,HO),HM],HY=[0,a(h[1][22],HX),HL];function
HZ(a){return 0}var
H0=[0,[0,0,0,[0,a(h[1][22],[0,h[1][19],HZ]),HY]],HK];g(h[1][25],gy,0,H0);var
H1=0,H2=0;function
H3(a,c,b){return[0,[0,0,a]]}var
H4=a(h[1][6],gy),H6=a(h[1][16],H5),H7=b(h[1][20],h[1][19],H6),H8=[0,b(h[1][20],H7,H4),H3],H9=[0,a(h[1][22],H8),H2];function
H_(a,c,b){return[0,[0,1,a]]}var
H$=a(h[1][6],gy),Ib=a(h[1][16],Ia),Ic=b(h[1][20],h[1][19],Ib),Id=[0,b(h[1][20],Ic,H$),H_],Ie=[0,a(h[1][22],Id),H9];function
If(a){return 0}var
Ig=[0,[0,0,0,[0,a(h[1][22],[0,h[1][19],If]),Ie]],H1];g(h[1][25],kY,0,Ig);var
Ih=0,Ii=0;function
Ij(e,h,d,g,c,b,a,f){return[0,[0,b,a,c,d],e]}var
Ik=a(h[1][6],eZ),Im=a(h[1][16],Il),In=a(h[1][6],h[15][3]),Ip=a(h[1][16],Io),Iq=a(h[1][6],gx),Ir=a(h[1][6],eT),Is=a(h[1][6],kY),It=b(h[1][20],h[1][19],Is),Iu=b(h[1][20],It,Ir),Iv=b(h[1][20],Iu,Iq),Iw=b(h[1][20],Iv,Ip),Ix=b(h[1][20],Iw,In),Iy=b(h[1][20],Ix,Im),Iz=[0,b(h[1][20],Iy,Ik),Ij],IA=[0,[0,0,0,[0,a(h[1][22],Iz),Ii]],Ih];g(h[1][25],gz,0,IA);var
IB=0,IC=0;function
ID(a,c,b){return a}var
IE=a(h[1][6],gz),IF=a(h[1][10],IE),IH=a(h[1][16],IG),II=b(h[1][20],h[1][19],IH),IJ=[0,b(h[1][20],II,IF),ID],IK=[0,a(h[1][22],IJ),IC];function
IL(a){return 0}var
IM=[0,[0,0,0,[0,a(h[1][22],[0,h[1][19],IL]),IK]],IB];g(h[1][25],kZ,0,IM);var
IN=0,IO=0;function
IP(a,c,b){return[1,a]}var
IQ=a(h[1][6],cY),IS=a(h[1][16],IR),IT=b(h[1][20],h[1][19],IS),IU=[0,b(h[1][20],IT,IQ),IP],IV=[0,a(h[1][22],IU),IO];function
IW(b,a,d,c){return[0,a,b]}var
IX=a(h[1][6],kZ),IY=a(h[1][6],h[15][3]),IZ=0;function
I0(a,b){return a}var
I2=a(h[1][16],I1),I3=[0,b(h[1][20],h[1][19],I2),I0],I4=[0,a(h[1][22],I3),IZ];function
I5(a,b){return a}var
I7=a(h[1][16],I6),I8=[0,b(h[1][20],h[1][19],I7),I5],I9=[0,a(h[1][22],I8),I4],I_=a(h[1][17],I9),I$=b(h[1][20],h[1][19],I_),Ja=b(h[1][20],I$,IY),Jb=[0,b(h[1][20],Ja,IX),IW],Jc=[0,a(h[1][22],Jb),IV];function
Jd(c,f,b,e,d){return a(b,c)}var
Je=a(h[1][6],eZ),Jf=0;function
Jg(a,b){return a}var
Ji=a(h[1][16],Jh),Jj=[0,b(h[1][20],h[1][19],Ji),Jg],Jk=[0,a(h[1][22],Jj),Jf];function
Jl(a,b){return a}var
Jn=a(h[1][16],Jm),Jo=[0,b(h[1][20],h[1][19],Jn),Jl],Jp=[0,a(h[1][22],Jo),Jk],Jq=a(h[1][17],Jp),Jr=a(h[1][6],kX),Js=0;function
Jt(a,b){return a}var
Jv=a(h[1][16],Ju),Jw=[0,b(h[1][20],h[1][19],Jv),Jt],Jx=[0,a(h[1][22],Jw),Js];function
Jy(a,b){return a}var
JA=a(h[1][16],Jz),JB=[0,b(h[1][20],h[1][19],JA),Jy],JC=[0,a(h[1][22],JB),Jx],JD=a(h[1][17],JC),JE=b(h[1][20],h[1][19],JD),JF=b(h[1][20],JE,Jr),JG=b(h[1][20],JF,Jq),JH=[0,b(h[1][20],JG,Je),Jd],JI=[0,a(h[1][22],JH),Jc];function
JJ(b,f,a,e,d,c){return[4,[0,a],b]}var
JK=a(h[1][6],eZ),JM=a(h[1][16],JL),JN=a(h[1][6],M[6][18]),JP=a(h[1][16],JO),JR=a(h[1][16],JQ),JS=b(h[1][20],h[1][19],JR),JT=b(h[1][20],JS,JP),JU=b(h[1][20],JT,JN),JV=b(h[1][20],JU,JM),JW=[0,b(h[1][20],JV,JK),JJ],JX=[0,a(h[1][22],JW),JI];function
JY(d,h,c,b,a,g,f,e){return[2,a,b,c,d]}var
JZ=a(h[1][6],dH),J0=0;function
J1(a,b){return a}var
J3=a(h[1][16],J2),J4=[0,b(h[1][20],h[1][19],J3),J1],J5=[0,a(h[1][22],J4),J0];function
J6(a,b){return a}var
J8=a(h[1][16],J7),J9=[0,b(h[1][20],h[1][19],J8),J6],J_=[0,a(h[1][22],J9),J5],J$=a(h[1][17],J_),Ka=a(h[1][6],cY),Kb=a(h[1][12],Ka),Kc=a(h[1][6],h[15][1]),Kd=a(h[1][12],Kc),Ke=a(h[1][6],h[15][1]),Kg=a(h[1][16],Kf),Ki=a(h[1][16],Kh),Kj=b(h[1][20],h[1][19],Ki),Kk=b(h[1][20],Kj,Kg),Kl=b(h[1][20],Kk,Ke),Km=b(h[1][20],Kl,Kd),Kn=b(h[1][20],Km,Kb),Ko=b(h[1][20],Kn,J$),Kp=[0,b(h[1][20],Ko,JZ),JY],Kq=[0,[0,0,0,[0,a(h[1][22],Kp),JX]],IN];g(h[1][25],gA,0,Kq);var
Kr=0,Ks=0;function
Kt(d,a,c,b){return a}var
Kv=a(h[1][16],Ku),Kw=a(h[1][6],dH),Ky=a(h[1][16],Kx),Kz=b(h[1][20],h[1][19],Ky),KA=b(h[1][20],Kz,Kw),KB=[0,b(h[1][20],KA,Kv),Kt],KC=[0,a(h[1][22],KB),Ks];function
KD(a,b){return a}var
KE=a(h[1][6],dH),KF=[0,b(h[1][20],h[1][19],KE),KD],KG=[0,[0,0,0,[0,a(h[1][22],KF),KC]],Kr];g(h[1][25],eZ,0,KG);var
KH=0,KI=0;function
KJ(a,b){return a}var
KK=a(h[1][6],gz),KL=a(h[1][10],KK),KM=[0,b(h[1][20],h[1][19],KL),KJ],KN=[0,[0,0,0,[0,a(h[1][22],KM),KI]],KH];g(h[1][25],kM,0,KN);var
KO=0,KQ=[0,[0,0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(n[4],ba),h=b(n[8],g,f),i=a(n[4],cj),j=b(n[8],i,e);return function(b,a){kv(h,j,0);return a}}}return a(E[3],KP)}],KO];function
KR(b,a){return g(gu[2],a[1],[0,KS,b],a[2])}b(F[89],KR,KQ);var
KT=0,KV=[0,function(b){if(b){var
c=b[2];if(c)if(!c[2])return function(a){return cX[5]}}return a(E[3],KU)},KT];function
KW(c,a){return b(cX[3],[0,KX,c],a)}b(F[89],KW,KV);var
KY=[6,a(h[12],cj)],KZ=[0,[0,a(n[4],cj)],KY],K0=[0,[1,b(J[11],0,KZ)],0],K1=[6,a(h[12],ba)],K2=[0,[0,a(n[4],ba)],K1],K4=[0,[0,K3,[0,[1,b(J[11],0,K2)],K0]],0];function
K5(b,a){return g(gv[1],[0,K6,b],0,a)}b(F[89],K5,K4);function
e0(e,d,c,b){return a(f[7],0)}var
bp=a(n[2],K7);function
K8(b,a){return[0,b,a]}b(a$[9],bp,K8);function
K9(b,a){return a}b(a$[10],bp,K9);function
K_(g,c){var
d=a(n[6],bp),e=a(Y[3],d),f=b(Y[1][8],e,c);return a(b1[1],f)}b(Y[7],bp,K_);b(Y[4],bp,0);b(h[11],bp,dI);o(M[5][1],bp,e0,e0,e0);var
K$=[0,dI,0];function
La(c){var
d=c[2],e=a(n[4],bp);return[0,b(n[7],e,d)]}g(M[10][5],Lb,La,K$);var
Lc=0;function
Ld(c,b,d){return gs([0,b],[0,a(J[3],Le),c])}var
Lg=a(j[1][7],Lf),Lh=[0,[5,a(n[16],bp)],Lg],Lj=[0,Li,[1,b(J[11],0,Lh),0]],Ll=a(j[1][7],Lk),Lm=[0,[5,a(n[16],D[8])],Ll],Lp=[0,[0,[0,Lo,[0,Ln,[1,b(J[11],0,Lm),Lj]]],Ld],Lc];function
Lq(b,c){return gs(0,[0,a(J[3],Lr),b])}var
Lt=a(j[1][7],Ls),Lu=[0,[5,a(n[16],D[8])],Lt],Lx=[0,[0,[0,Lw,[0,Lv,[1,b(J[11],0,Lu),0]]],Lq],Lp];o(M[10][8],am,Ly,0,Lx);var
Lz=0;function
LA(e,g){function
d(g){var
h=a(k[66][6],g),d=b(c[3],h,e);if(1===d[0])if(a(r[e6],d[1]))return a(k[16],0);var
i=a(f[3],LB);return b(m[66][4],0,i)}return a(k[66][10],d)}var
LD=a(j[1][7],LC),LE=[0,[5,a(n[16],D[13])],LD],LG=[0,[0,[0,LF,[1,b(J[11],0,LE),0]],LA],Lz];o(M[10][8],am,LH,0,LG);var
LI=0;function
LJ(a,b){return kz(a)}var
LL=a(j[1][7],LK),LM=[0,[5,a(n[16],D[15])],LL],LO=[0,[0,[0,LN,[1,b(J[11],0,LM),0]],LJ],LI];o(M[10][8],am,LP,0,LO);var
LQ=0;function
LR(a,d){function
c(b){return kn(a,b)}return b(k[70][1],0,c)}var
LT=a(j[1][7],LS),LU=[0,[5,a(n[16],D[8])],LT],LW=[0,[0,[0,LV,[1,b(J[11],0,LU),0]],LR],LQ];o(M[10][8],am,LX,0,LW);var
LY=0;function
LZ(b,a,c){return h1(b,a)}var
L1=a(j[1][7],L0),L2=[0,[5,a(n[16],D[13])],L1],L3=[1,b(J[11],0,L2),0],L5=a(j[1][7],L4),L6=[0,[5,a(n[16],D[8])],L5],L8=[0,[0,[0,L7,[1,b(J[11],0,L6),L3]],LZ],LY];o(M[10][8],am,L9,0,L8);var
L_=0,Ma=[0,[0,0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
f=d[1],g=c[1],h=a(n[18],D[8]),i=a(n[4],h),k=b(n[8],i,g),l=a(n[18],D[24]),m=a(n[4],l),o=b(n[8],m,f);return function(c,a){function
d(a){var
c=b(cr[3],0,a);return[0,a[2],c]}var
f=b(e[17][15],d,o),g=b(e[17][15],j[1][8],k);ig(c[3],g,f);return a}}}return a(E[3],L$)}],L_];function
Mb(b,a){return g(gu[2],a[1],[0,Mc,b],a[2])}b(F[89],Mb,Ma);var
Md=0,Mf=[0,function(b){if(b){var
c=b[2];if(c)if(!c[2])return function(a){return cX[5]}}return a(E[3],Me)},Md];function
Mg(c,a){return b(cX[3],[0,Mh,c],a)}b(F[89],Mg,Mf);var
Mi=[3,[6,a(h[12],D[24])]],Mj=a(n[18],D[24]),Mk=[0,[0,a(n[4],Mj)],Mi],Mm=[0,Ml,[0,[1,b(J[11],0,Mk)],0]],Mn=[1,[6,a(h[12],D[8])]],Mo=a(n[18],D[8]),Mp=[0,[0,a(n[4],Mo)],Mn],Mr=[0,[0,Mq,[0,[1,b(J[11],0,Mp)],Mm]],0];function
Ms(b,a){return g(gv[1],[0,Mt,b],0,a)}b(F[89],Ms,Mr);var
e1=a(n[3],Mu),Mv=b(Y[4],e1,0);function
k0(c,b,a){return dB}function
k1(c,b,a){return dB}function
k2(c,b,a){return dB}var
Mw=a(n[4],e1),cZ=g(h[13],h[9],Mx,Mw);o(M[5][1],e1,k0,k1,k2);var
e2=h[1][4][1],k3=a(e2,My),k4=a(e2,Mz),k5=a(e2,MA),k6=a(e2,MB),MC=0,MD=0;function
ME(a,b){return a}var
MF=a(h[1][6],k3),MG=a(h[1][10],MF),MH=[0,b(h[1][20],h[1][19],MG),ME],MI=[0,[0,0,0,[0,a(h[1][22],MH),MD]],MC];g(h[1][25],cZ,0,MI);var
MJ=0,MK=0;function
ML(c,b){return[0,[0,a(h[29],b)],c]}var
MM=a(h[1][6],k4),MN=[0,b(h[1][20],h[1][19],MM),ML],MO=[0,[0,0,0,[0,a(h[1][22],MN),MK]],MJ];g(h[1][25],k3,0,MO);var
MP=0,MQ=0;function
MR(a,b){return[0,a]}var
MS=a(h[1][6],k5),MT=[0,b(h[1][20],h[1][19],MS),MR],MU=[0,a(h[1][22],MT),MQ];function
MV(b,a){return 0}var
MX=a(h[1][16],MW),MY=[0,b(h[1][20],h[1][19],MX),MV],MZ=[0,a(h[1][22],MY),MU];function
M0(b,a){return 1}var
M2=a(h[1][16],M1),M3=[0,b(h[1][20],h[1][19],M2),M0],M4=[0,a(h[1][22],M3),MZ];function
M5(b,a){return 2}var
M7=a(h[1][16],M6),M8=[0,b(h[1][20],h[1][19],M7),M5],M9=[0,[0,0,0,[0,a(h[1][22],M8),M4]],MP];g(h[1][25],k4,0,M9);var
M_=0,M$=0;function
Na(b,a){return Nb}var
Nd=a(h[1][16],Nc),Ne=[0,b(h[1][20],h[1][19],Nd),Na],Nf=[0,a(h[1][22],Ne),M$];function
Ng(b,a){return Nh}var
Nj=a(h[1][16],Ni),Nk=[0,b(h[1][20],h[1][19],Nj),Ng],Nl=[0,a(h[1][22],Nk),Nf];function
Nm(b,a){return Nn}var
Np=a(h[1][16],No),Nq=[0,b(h[1][20],h[1][19],Np),Nm],Nr=[0,a(h[1][22],Nq),Nl];function
Ns(e,a,d,c,b){return[2,a]}var
Nu=a(h[1][16],Nt),Nv=a(h[1][6],cZ),Nx=a(h[1][16],Nw),Nz=a(h[1][16],Ny),NA=b(h[1][20],h[1][19],Nz),NB=b(h[1][20],NA,Nx),NC=b(h[1][20],NB,Nv),ND=[0,b(h[1][20],NC,Nu),Ns],NE=[0,a(h[1][22],ND),Nr];function
NF(a,b){return[1,a]}var
NG=a(h[1][6],k6),NH=[0,b(h[1][20],h[1][19],NG),NF],NI=[0,[0,0,0,[0,a(h[1][22],NH),NE]],M_];g(h[1][25],k5,0,NI);var
NJ=0,NK=0;function
NL(b,a){return 0}var
NN=a(h[1][16],NM),NO=[0,b(h[1][20],h[1][19],NN),NL],NP=[0,a(h[1][22],NO),NK];function
NQ(b,a){return 1}var
NS=a(h[1][16],NR),NT=[0,b(h[1][20],h[1][19],NS),NQ],NU=[0,[0,0,0,[0,a(h[1][22],NT),NP]],NJ];g(h[1][25],k6,0,NU);function
e3(c,b,a){return dB}var
bq=a(n[2],NV);function
NW(b,a){return[0,b,a]}b(a$[9],bq,NW);function
NX(b,a){return a}b(a$[10],bq,NX);function
NY(g,c){var
d=a(n[6],bq),e=a(Y[3],d),f=b(Y[1][8],e,c);return a(b1[1],f)}b(Y[7],bq,NY);b(Y[4],bq,0);b(h[11],bq,cZ);o(M[5][1],bq,e3,e3,e3);var
NZ=[0,cZ,0];function
N0(c){var
d=c[2],e=a(n[4],bq);return[0,b(n[7],e,d)]}g(M[10][5],N1,N0,NZ);var
N2=0,N4=[0,[0,N3,function(a){return f_(0)}],N2];function
N5(a,b){return f_(a)}var
N7=a(j[1][7],N6),N8=[0,[5,a(n[16],bq)],N7],N_=[0,[0,[0,N9,[1,b(J[11],0,N8),0]],N5],N4];o(M[10][8],am,N$,0,N_);var
Oa=0;function
Ob(b,a,c){return gh(b,a)}var
Od=a(j[1][7],Oc),Oe=[0,[2,[5,a(n[16],D[3])]],Od],Of=[1,b(J[11],0,Oe),0],Oh=a(j[1][7],Og),Oi=[0,[2,[5,a(n[16],eX)]],Oh],Ok=[0,[0,[0,Oj,[1,b(J[11],0,Oi),Of]],Ob],Oa];o(M[10][8],am,Ol,0,Ok);aH(1112,[0,am,eQ,bI,eR,ba,gw,eS,bJ,eT,EO,EP,kA,kB,kC,eU,gx,ET,eV,EV,kD,kE,kF,dH,eW,EZ,kG,kH,kI,dI,cj,E3,kJ,kK,kL,kM,E6,kN,eX,kO,kP,kQ,kR,kS,kT,e0,bp,dI,e1,Mv,k0,k1,k2,cZ,e3,bq,cZ],"G_equations");a(aX[10],Om);a(aX[10],On);a(aX[10],Oo);a(aX[10],Op);a(aX[10],Oq);a(aX[10],Or);a(aX[10],Os);a(aX[10],Ot);a(aX[10],Ou);a(aX[10],Ov);a(aX[10],Ow);a(aX[10],Ox);a(aX[10],Oy);aH(1113,[0],"Equations_plugin_mod");return}
