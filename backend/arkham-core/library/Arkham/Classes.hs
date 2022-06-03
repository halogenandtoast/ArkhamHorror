{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Arkham.Classes
  ( module Arkham.Classes
  , module X
  ) where

import Arkham.Prelude hiding (to)

import Arkham.Ability
import Arkham.Action hiding (Ability)
import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes.Depth as X
import Arkham.Classes.Query as X
import Arkham.Classes.HasSet as X
import Arkham.Classes.Entity as X
import Arkham.Classes.GameLogger as X
import Arkham.Classes.HasQueue as X
import Arkham.Classes.HasRecord as X
import Arkham.Classes.HasModifiersFor as X
import Arkham.Classes.HasHistory as X
import Arkham.Classes.HasTokenValue as X
import Arkham.Classes.RunMessage as X
import Arkham.Id
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Name
import Arkham.Phase
import Arkham.Query
import Arkham.SkillType
import Arkham.Source
import Arkham.Stats
import Arkham.Trait
import Data.Char qualified as C
import Language.Haskell.TH.Syntax hiding (Name)
import Language.Haskell.TH.Syntax qualified as TH

newtype Distance = Distance { unDistance :: Int }
  deriving newtype (Ord, Eq)

class Monad m => HasPhase m where
  getPhase :: m Phase

class Monad m => HasStep step m a where
  getStep :: a -> m step

class Monad m => HasList list m a where
  getList :: a -> m [list]

class Monad m => HasId id m a where
  getId :: a -> m id

class Monad m => HasCount count m a where
  getCount :: a -> m count

getDistance :: (HasCount (Maybe Distance) m (LocationId, LocationId)) => LocationId -> LocationId -> m (Maybe Distance)
getDistance lid1 lid2 = getCount (lid1, lid2)

class Monad m => HasName m a where
  getName :: a -> m Name

class Monad m => HasPlayerCard m a where
  getPlayerCard :: a -> m (Maybe PlayerCard)

type HasCostPayment m
  = ( HasCount ActionRemainingCount m (Maybe Action, [Trait], InvestigatorId)
    , HasCount PlayerCount m ()
    , HasCount ResourceCount m InvestigatorId
    , HasCount SpendableClueCount m ()
    , HasCount SpendableClueCount m InvestigatorId
    , HasCount UsesCount m AssetId
    , HasList InPlayCard m InvestigatorId
    , HasId (Maybe LocationId) m LocationMatcher
    , HasList HandCard m InvestigatorId
    , HasList TakenAction m InvestigatorId
    , Query AssetMatcher m
    , Query EventMatcher m
    , Query InvestigatorMatcher m
    , Query ExtendedCardMatcher m
    , HasSet InvestigatorId m LocationId
    )

class Monad m => HasStats m a where
  getStats :: a -> Source -> m Stats

class Monad m => HasSkillValue m a where
  getSkillValue :: HasModifiersFor m () => SkillType -> a -> m Int

class HasVictoryPoints a where
  getVictoryPoints :: a -> Maybe Int

class HasDamage a where
  getDamage :: a -> (Int, Int)

class HasTrauma a where
  getTrauma :: a -> (Int, Int)

instance HasVictoryPoints Card where
  getVictoryPoints (PlayerCard card) = getVictoryPoints card
  getVictoryPoints (EncounterCard card) = getVictoryPoints card

instance HasVictoryPoints EncounterCard where
  getVictoryPoints = cdVictoryPoints . toCardDef

instance HasVictoryPoints PlayerCard where
  getVictoryPoints = cdVictoryPoints . toCardDef

type ActionRunner m
  = ( HasTokenValue m ()
    , HasSet LocationId m LocationMatcher
    , HasSet TreacheryId m LocationId
    , HasSet FarthestLocationId m (InvestigatorId, LocationMatcher)
    , HasList TakenAction m InvestigatorId
    , Query AssetMatcher m
    , Query LocationMatcher m
    , GetCardDef m EnemyId
    , HasCostPayment m
    , ( HasCount
          ActionRemainingCount
          m
          (Maybe Action, [Trait], InvestigatorId)
      , HasCount ActionRemainingCount m InvestigatorId
      , HasCount ActionTakenCount m InvestigatorId
      , HasCount CardCount m InvestigatorId
      , HasCount ClueCount m InvestigatorId
      , HasCount ClueCount m LocationId
      , HasCount DamageCount m InvestigatorId
      , HasCount DoomCount m AssetId
      , HasCount DoomCount m InvestigatorId
      , HasCount HorrorCount m InvestigatorId
      , HasCount SetAsideCount m CardCode
      )
    , HasId (Maybe LocationId) m AssetId
    , HasId CardCode m EnemyId
    , HasId LeadInvestigatorId m ()
    , HasId LocationId m InvestigatorId
    , HasList CommittedCard m InvestigatorId
    , HasList CommittedSkillIcon m InvestigatorId
    , HasList DiscardedPlayerCard m InvestigatorId
    , HasList InPlayCard m InvestigatorId
    , HasList UnderneathCard m InvestigatorId
    , HasList UsedAbility m ()
    , HasModifiersFor m ()
    , HasSet AccessibleLocationId m LocationId
    , HasSet CommittedCardId m InvestigatorId
    , HasSet ConnectedLocationId m LocationId
    , HasSet EnemyId m ([Trait], LocationId)
    , HasSet EnemyId m CardCode
    , HasSet EnemyId m EnemyMatcher
    , HasSet EnemyId m InvestigatorId
    , HasSet EnemyId m LocationId
    , HasSet FightableEnemyId m (InvestigatorId, Source)
    , HasSet InvestigatorId m ()
    , HasSet InvestigatorId m (HashSet LocationId)
    , HasSet InvestigatorId m EnemyId
    , HasSet Keyword m EnemyId
    , HasSet LocationId m ()
    , HasSet LocationId m [Trait]
    , HasSet Trait m (InvestigatorId, CardId)
    , HasSet Trait m EnemyId
    , HasSet Trait m LocationId
    , HasSet Trait m Source
    , HasStep ActStep m ()
    )

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities = const []

class Discardable a where
  canBeDiscarded :: a -> Bool

class Exhaustable a where
  isExhausted :: a -> Bool
  isReady :: a -> Bool

  isExhausted = not . isReady
  isReady = not . isExhausted
  {-# MINIMAL isExhausted | isReady #-}

buildEntity :: String -> Q [Dec]
buildEntity nm = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  let conz = mapMaybe extractCon instances
  pure
    [ DataD
        []
        (TH.mkName nm)
        []
        Nothing
        conz
        [ DerivClause (Just StockStrategy) (map ConT [''Show, ''Eq])
        ]
    ]
 where
  extractCon (InstanceD _ _ (AppT _ con@(ConT name)) _) = Just $ NormalC
    (TH.mkName $ nameBase name ++ "'")
    [(Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, con)]
  extractCon _ = Nothing

buildEntityLookupList :: String -> Q Exp
buildEntityLookupList nm = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  let conz = mapMaybe extractCon instances
  pure $ ListE conz
 where
  extractCon (InstanceD _ _ (AppT _ (ConT name)) _) = Just $ AppE
    (AppE (VarE $ TH.mkName "fmap") (ConE $ TH.mkName $ nameBase name ++ "'"))
    (VarE $ toFunName $ nameBase name)
  extractCon _ = Nothing
  toFunName [] = TH.mkName ""
  toFunName (x : xs) = TH.mkName $ C.toLower x : xs

-- entityRunMessage :: Message -> a -> m a
-- (a -> b) -> a -> b

entityRunMessage :: String -> Q Exp
entityRunMessage nm = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  a <- newName "a"
  msg <- newName "msg"
  x <- newName "x"
  let matches = mapMaybe (toMatch msg x) instances
  pure $ LamE [VarP msg, VarP a] $ CaseE (VarE a) matches
 where
  toMatch msg x (InstanceD _ _ (AppT _ (ConT name)) _) = Just $ Match
    (ConP (TH.mkName $ nameBase name <> "'")  [VarP x])
    (NormalB $ AppE (AppE (VarE $ TH.mkName "fmap") (ConE $ TH.mkName $ nameBase name ++ "'")) (AppE (AppE (VarE $ TH.mkName "runMessage") (VarE msg) ) (VarE x)))
    []
  toMatch _ _ _ = Nothing

entityF :: String -> String -> Q Exp
entityF nm fName = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  let f = TH.mkName fName
  a <- newName "a"
  x <- newName "x"
  let matches = mapMaybe (toMatch f x) instances
  pure $ LamE [VarP a] $ CaseE (VarE a) matches
 where
  toMatch f x (InstanceD _ _ (AppT _ (ConT name)) _) = Just $ Match
    (ConP (TH.mkName $ nameBase name <> "'")  [VarP x])
    (NormalB $ AppE (VarE f) (VarE x))
    []
  toMatch _ _  _ = Nothing

entityF2 :: String -> String -> Q Exp
entityF2 nm fName = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  let f = TH.mkName fName
  a <- newName "a"
  p1 <- newName "p1"
  p2 <- newName "p2"
  x <- newName "x"
  let matches = mapMaybe (toMatch f p1 p2 x) instances
  pure $ LamE [VarP p1, VarP p2, VarP a] $ CaseE (VarE a) matches
 where
  toMatch f p1 p2 x (InstanceD _ _ (AppT _ (ConT name)) _) = Just $ Match
    (ConP (TH.mkName $ nameBase name <> "'")  [VarP x])
    (NormalB $
      AppE
        (AppE
          (AppE
            (VarE f)
            (VarE p1)
          )
          (VarE p2))
        (VarE x)
    )

    []
  toMatch _ _ _ _ _ = Nothing

entityF1 :: String -> String -> Q Exp
entityF1 nm fName = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  let f = TH.mkName fName
  a <- newName "a"
  p1 <- newName "p1"
  x <- newName "x"
  let matches = mapMaybe (toMatch f p1 x) instances
  pure $ LamE [VarP p1, VarP a] $ CaseE (VarE a) matches
 where
  toMatch f p1 x (InstanceD _ _ (AppT _ (ConT name)) _) = Just $ Match
    (ConP (TH.mkName $ nameBase name <> "'")  [VarP x])
    (NormalB $
      AppE
        (AppE
          (VarE f)
          (VarE p1)
        )
        (VarE x)
    )

    []
  toMatch _ _ _ _ = Nothing
