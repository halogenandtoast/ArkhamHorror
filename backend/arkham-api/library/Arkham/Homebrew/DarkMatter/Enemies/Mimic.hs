module Arkham.Homebrew.DarkMatter.Enemies.Mimic (mimic) where

import {-# SOURCE #-} Arkham.Asset qualified as Asset
import Arkham.Asset.Types (assetSanity)
import Arkham.Card
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorDiscard))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Ally))
import Data.UUID qualified as UUID

newtype Mimic = Mimic EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mimic :: EnemyCard Mimic
mimic = enemy Mimic Cards.mimic

{- | "Mimic gains the traits of the topmost [[Ally]] asset in your discard pile. /
Mimic deals additional horror equal to the sanity value of the topmost [[Ally]]
asset in your discard pile."
-}
instance HasModifiersFor Mimic where
  getModifiersFor (Mimic a) = do
    engaged <- select $ InvestigatorEngagedWith (EnemyWithId a.id)
    for_ engaged \iid -> do
      pile <- field InvestigatorDiscard iid
      case find (`cardMatch` (CardWithType AssetType <> CardWithTrait Ally)) (map PlayerCard pile) of
        Nothing -> pure ()
        Just ally -> do
          {- The ally's printed sanity is not on its CardDef — asset health/sanity
          live in the asset's implementation — so the asset is built off-board
          from its card code purely to read that printed value. -}
          let sanity =
                fromMaybe 0
                  $ assetSanity
                  $ toAttrs
                  $ Asset.createAsset ally (AssetId UUID.nil)
          modifySelf a
            $ map AddTrait (setToList $ cdCardTraits $ toCardDef ally)
            <> [HorrorDealt sanity | sanity > 0]

instance RunMessage Mimic where
  runMessage msg (Mimic attrs) = Mimic <$> runMessage msg attrs
