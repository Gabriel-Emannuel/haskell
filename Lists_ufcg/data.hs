data Quadruple a b = Quadruple a a b b

firstTwo :: Quadruple t z -> (t, t)
firstTwo (Quadruple a b _ _)= (a, b)

secondTwo :: Quadruple t z -> (z, z)
secondTwo (Quadruple _ _ a b) = (a, b)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Four a b c d | Three a b c | Two a b | One a  

tuple1 (Four a _ _ _) = Just a  
tuple1 (Three a _ _ ) = Just a  
tuple1 (Two a _  ) = Just a  
tuple1 (One a) = Just a  


tuple2 (Four _ a _ _) = Just a  
tuple2 (Three _ a _ ) = Just a  
tuple2 (Two _ a  ) = Just a  
tuple2 (One _) = Nothing

tuple3 (Four _ _ a _) = Just a  
tuple3 (Three _ _ a ) = Just a  
tuple3 (Two _ _  ) = Nothing  
tuple3 (One _) = Nothing  

tuple4 (Four _ _ _ a) = Just a  
tuple4 (Three _ _ _ ) = Nothing  
tuple4 (Two _ _  ) = Nothing  
tuple4 (One _) = Nothing  

--Escreva as funcoes sobre a estrutura de dados binary tree

-- AO FINAL DAS IMPLEMENTACOES, VOCE PRECISA MODULARIZAR O SEU CÓDIGO ACIMA DA SEGUINTE FORMA
-- COLOQUE TODA A IMPLEMENTACAO DE LISTA EM UM MODULO CHAMADO MYLIST
-- COLOQUE TODA A IMPLEMENTACAO DE ARVORE BINARIA EM UM MODULO CHAMADO BST