([L  (VarBind {var_ext = NoExtField, var_id = $trModule, var_rhs = 
    L  (HsApp  (
        L  (HsApp  (
            L  (HsConLikeOut NoExtField )) (
                L  (HsPar  (
                    L  (HsApp  (
                        L  (HsConLikeOut NoExtField )) (
                            L  (HsLit  "main")))))))) (
                                L  (HsPar  (
                                    L  (HsApp  (
                                        L  (HsConLikeOut NoExtField )) (
                                            L  (HsLit  "Test3")))))))}),
    L  (AbsBinds {abs_ext = NoExtField, abs_tvs = [], abs_ev_vars = [], abs_exports = [], abs_ev_binds = [EvBinds{}], abs_binds = [
        L  (FunBind {fun_ext = <>, fun_id = 
            L  f, fun_matches = MG {mg_ext = , mg_alts = 
                L  [L  (Match {m_ext = , m_ctxt = FunRhs {mc_fun = 
                    L  f, mc_fixity = Prefix, mc_strictness = NoSrcStrict}, m_pats = [
                        L  ds_a1xrb], m_grhss = GRHSs {grhssExt = , grhssGRHSs = [
                            L (RealSrcSpan SrcSpanOneLine "other/good/Test3.hs" 5 6 21 (Just (BufSpan {bufSpanStart = BufPos {bufPos = 45}, bufSpanEnd = BufPos {bufPos = 60}}))) (GRHS  [
                                L  (BodyStmt (TyConApp Bool []) (
                                    L  (XExpr )) <no syntax expr> <no syntax expr>)] (
                                        L  (HsOverLit  IL {il_text = SourceText "1", il_neg = False, il_value = 1}HsLit  IL {il_text = SourceText "1", il_neg = False, il_value = 1}))),
                                        L (RealSrcSpan SrcSpanOneLine "other/good/Test3.hs" 6 6 21 (Just (BufSpan {bufSpanStart = BufPos {bufPos = 66}, bufSpanEnd = BufPos {bufPos = 81}}))) (GRHS  [
                                            L  (BodyStmt (TyConApp Bool []) (
                                                L  (XExpr )) <no syntax expr> <no syntax expr>)] (
                                                    L  (HsOverLit  IL {il_text = SourceText "2", il_neg = False, il_value = 2}HsLit  IL {il_text = SourceText "2", il_neg = False, il_value = 2}))),
                                                    L (RealSrcSpan SrcSpanOneLine "other/good/Test3.hs" 7 6 21 (Just (BufSpan {bufSpanStart = BufPos {bufPos = 87}, bufSpanEnd = BufPos {bufPos = 102}}))) (GRHS  [
                                                        L  (BodyStmt (TyConApp Bool []) (
                                                            L  (XExpr )) <no syntax expr> <no syntax expr>)] (
                                                                L  (HsOverLit  IL {il_text = SourceText "3", il_neg = False, il_value = 3}HsLit  IL {il_text = SourceText "3", il_neg = False, il_value = 3}))),
                                                                L (RealSrcSpan SrcSpanOneLine "other/good/Test3.hs" 8 6 21 (Just (BufSpan {bufSpanStart = BufPos {bufPos = 108}, bufSpanEnd = BufPos {bufPos = 123}}))) (GRHS  [
                                                                    L  (BodyStmt (TyConApp Bool []) (
                                                                        L  (XExpr )) <no syntax expr> <no syntax expr>)] (
                                                                            L  (HsOverLit  IL {il_text = SourceText "4", il_neg = False, il_value = 4}HsLit  IL {il_text = SourceText "4", il_neg = False, il_value = 4})))], grhssLocalBinds = EmptyLocalBinds NoExtField}})], mg_origin = FromSource}, fun_tick = []})], abs_sig = True})],[])
