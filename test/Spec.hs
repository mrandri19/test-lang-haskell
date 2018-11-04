import           Control.Exception    (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import           Text.Megaparsec      (parse)

import           Infer                (InferError (..), emptyContext, infer)
import           Parser               (parseExpr)
import           TreeWalkingEvaluator (Value (..), eval)
import           Type

import           Syntax

main :: IO ()
main =
  hspec $ do
    let filename = "Dummy filename for the tests"
    let p = parse parseExpr filename
    describe "Parser" $ do
      it "parses a number" $ p "1" `shouldBe` Right (Number 1)
      it "parses an identifier" $ p "id" `shouldBe` Right (Var "id")
      it "parses booleans" $ do
        p "true" `shouldBe` Right (Boolean True)
        p "false" `shouldBe` Right (Boolean False)
      it "ignores whitespace" $ do
        p " 1 " `shouldBe` Right (Number 1)
        p "        dawdaw " `shouldBe` Right (Var "dawdaw")
      it "parses addition " $
        p "1 + 2" `shouldBe` Right (BinOp Plus (Number 1) (Number 2))
      it "parses parentheses " $
        p "(1 + (2+3))" `shouldBe`
        Right (BinOp Plus (Number 1) (BinOp Plus (Number 2) (Number 3)))
      it "parses subtraction " $
        p "1-2" `shouldBe` Right (BinOp Minus (Number 1) (Number 2))
      it "parses multiplication " $
        p "1*2" `shouldBe` Right (BinOp Times (Number 1) (Number 2))
      it "parses division " $
        p "1/2" `shouldBe` Right (BinOp Divide (Number 1) (Number 2))
      it "parses comparisons" $ do
        p "1>2" `shouldBe` Right (BinOp Greater (Number 1) (Number 2))
        p "1<2" `shouldBe` Right (BinOp Lesser (Number 1) (Number 2))
        p "2>1+1" `shouldBe`
          Right (BinOp Greater (Number 2) (BinOp Plus (Number 1) (Number 1)))
      it "parses precendeces correctly " $ do
        p "1+2*3" `shouldBe`
          Right (BinOp Plus (Number 1) (BinOp Times (Number 2) (Number 3)))
        p "(1+2)*3" `shouldBe`
          Right (BinOp Times (BinOp Plus (Number 1) (Number 2)) (Number 3))
      it "parses let expressions" $
        p "let a = 1 in a+2" `shouldBe`
        Right (LetBind "a" (Number 1) (BinOp Plus (Var "a") (Number 2)))
      it "parses function declarations" $
        p "fun x -> x" `shouldBe` Right (Lambda "x" (Var "x"))
      it "parses function applications" $ do
        p "(fun x -> x) 1" `shouldBe`
          Right (Apply (Lambda "x" (Var "x")) (Number 1))
        p "add 1 2 * 4" `shouldBe`
          Right
            (BinOp
               Times
               (Apply (Apply (Var "add") (Number 1)) (Number 2))
               (Number 4))
      it "parses if expressions" $ do
        p "if 1 then 2 else 3" `shouldBe`
          Right (IfExpr (Number 1) (Number 2) (Number 3))
        p "if true then 2 else 3" `shouldBe`
          Right (IfExpr (Boolean True) (Number 2) (Number 3))
        p "if 1>2 then 3 else 4" `shouldBe`
          Right
            (IfExpr (BinOp Greater (Number 1) (Number 2)) (Number 3) (Number 4))
    describe "TreeWalkingEvaluator" $ do
      it "evaluates a number" $ eval (Number 1) [] `shouldBe` VNumber 1
      it "evaluates a boolean" $ eval (Boolean True) [] `shouldBe` VBoolean True
      it "evaluates a variable" $ do
        eval (Var "x") [("x", VNumber 42)] `shouldBe` VNumber 42
        evaluate (eval (Var "x") []) `shouldThrow` anyErrorCall
      it "evaluates a binary operation" $ do
        eval (BinOp Plus (Number 1) (Number 2)) [] `shouldBe` VNumber 3
        eval (BinOp Divide (Number 1) (Number 2)) [] `shouldBe` VNumber 0
        eval (BinOp Greater (Number 1) (Number 2)) [] `shouldBe` VBoolean False
        evaluate (eval (BinOp Plus (Number 1) (Boolean True)) []) `shouldThrow`
          anyException
      it "evaluates nested binary expressions" $ do
        eval (BinOp Plus (Number 1) (BinOp Plus (Number 2) (Number 3))) [] `shouldBe`
          VNumber 6
        evaluate
          (eval (BinOp Plus (Number 1) (BinOp Lesser (Number 1) (Number 2))) []) `shouldThrow`
          anyException
      it "evaluates if expressions" $ do
        eval (IfExpr (Boolean True) (Number 1) (Number 2)) [] `shouldBe`
          VNumber 1
        eval (IfExpr (Boolean False) (Number 1) (Number 2)) [] `shouldBe`
          VNumber 2
        evaluate (eval (IfExpr (Number 1) (Number 2) (Number 3)) []) `shouldThrow`
          anyException
      it "evaluates let bindings" $
        eval (LetBind "x" (Number 1) (Var "x")) [] `shouldBe` VNumber 1
      it "evaluates lambdas" $
        eval (Lambda "x" (Var "x")) [] `shouldBe` VClosure "x" (Var "x") []
      it "evaluates lambda applications" $ do
        evaluate (eval (Apply (Number 2) (Number 1)) []) `shouldThrow`
          anyException
        eval (Apply (Lambda "x" (Var "x")) (Number 1)) [] `shouldBe` VNumber 1
      -- let id = fun x -> x in id 12
        eval
          (LetBind "id" (Lambda "x" (Var "x")) (Apply (Var "id") (Number 12)))
          [] `shouldBe`
          VNumber 12
      -- let id = fun x -> x+1 in id 12
        eval
          (LetBind
             "id"
             (Lambda "x" (BinOp Plus (Var "x") (Number 1)))
             (Apply (Var "id") (Number 12)))
          [] `shouldBe`
          VNumber 13
        case p "let id = (let a = 2 in fun x -> x+a) in id 12" of
          Right ast -> (eval ast []) `shouldBe` (VNumber 14)
        case p "(fun y -> fun x -> x+y) 1 2" of
          Right ast -> (eval ast []) `shouldBe` (VNumber 3)
        case p "let a = (fun y -> fun x -> x+y) 1 2 in a+1" of
          Right ast -> (eval ast []) `shouldBe` (VNumber 4)
    describe "Infer" $ do
      it "infers numbers" $
        infer (Number 2) emptyContext `shouldBe` (Right $ TyConst "Number")
      it "infers booleans" $ do
        infer (Boolean True) emptyContext `shouldBe` (Right $ tyBoolean)
        infer (Boolean False) emptyContext `shouldBe` (Right $ tyBoolean)
      it "infers expressions" $ do
        infer (BinOp Plus (Number 1) (Number 2)) emptyContext `shouldBe`
          (Right $ TyConst "Number")
        infer (BinOp Greater (Number 1) (Number 2)) emptyContext `shouldBe`
          (Right $ tyBoolean)
      it "infers if expressions" $ do
        infer
          (IfExpr (BinOp Greater (Number 1) (Number 2)) (Number 3) (Number 4))
          emptyContext `shouldBe`
          (Right $ TyConst "Number")
        infer
          (IfExpr
             (BinOp Greater (Number 1) (Number 2))
             (Boolean True)
             (Boolean False))
          emptyContext `shouldBe`
          (Right $ tyBoolean)
        infer
          (IfExpr (BinOp Plus (Number 1) (Number 2)) (Number 3) (Number 4))
          emptyContext `shouldBe`
          (Left $ IncompatibleTypes (TyConst "Number") tyBoolean)
      it "infers let expressions" $ do
        infer (LetBind "x" (Number 1) (Var "x")) emptyContext `shouldBe`
          (Right $ TyConst "Number")
        infer
          (LetBind
             "f"
             (Lambda "x" (Var "x"))
             (LetBind
                "g"
                (Apply (Var "f") (Boolean True))
                (Apply (Var "f") (Number 3))))
          emptyContext `shouldBe`
          (Right $ TyConst "Number")
      it "infers lambdas" $ do
        infer (Lambda "x" (Number 1)) emptyContext `shouldBe`
          (Right $ TyArrow (TyVar "0") (TyConst "Number"))
        infer (Lambda "x" (Boolean True)) emptyContext `shouldBe`
          (Right $ TyArrow (TyVar "0") tyBoolean)
        infer (Lambda "x" (Var "x")) emptyContext `shouldBe`
          (Right $ TyArrow (TyVar "0") (TyVar "0"))
      it "infers lambda applications" $ do
        infer (Apply (Lambda "x" (Number 1)) (Number 2)) emptyContext `shouldBe`
          (Right $ TyConst "Number")
        infer (Apply (Lambda "x" (Boolean False)) (Number 2)) emptyContext `shouldBe`
          (Right tyBoolean)
        infer (Apply (Number 1) (Number 2)) emptyContext `shouldSatisfy`
          (\x ->
             case x of
               Left v ->
                 case v of
                   IncompatibleTypes _ _ -> True
                   _                     -> False
               _ -> False)
        case p "let a = (fun y -> fun x -> x+y) 1 2 in a+1" of
          Right ast ->
            infer ast emptyContext `shouldBe` (Right $ TyConst "Number")
        infer (Apply (Lambda "x" (Var "x")) (Boolean True)) emptyContext `shouldBe`
          Right tyBoolean
