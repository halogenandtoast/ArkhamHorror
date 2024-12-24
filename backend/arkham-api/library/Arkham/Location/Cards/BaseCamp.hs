module Arkham.Location.Cards.BaseCamp (baseCamp) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhenM)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype BaseCamp = BaseCamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor BaseCamp where
  getModifiersFor (BaseCamp a) = do
    modifySelfWhenM
      a
      (selectNone $ SetAsideCardMatch $ cardsAre mirageCards)
      [ClearedOfMirages]

baseCamp :: LocationCard BaseCamp
baseCamp = location BaseCamp Cards.baseCamp 4 (PerPlayer 3)

mirageCards :: [CardDef]
mirageCards = [Cards.coastalWaters, Cards.desertedStation, Cards.riverviewTheatre]

instance HasAbilities BaseCamp where
  getAbilities (BaseCamp a) =
    extendRevealed
      a
      [ mirage a 1 mirageCards
      , mkAbility a 1
          $ ReactionAbility
            ( WouldHaveSkillTestResult #when You (WhileInvestigating $ be a) (FailureResult $ EqualTo $ Static 1)
            )
            (HorrorCost (a.ability 2) YouTarget 1)
      ]

instance RunMessage BaseCamp where
  runMessage msg l@(BaseCamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
        push RecalculateSkillTestResults
      pure l
    _ -> BaseCamp <$> mirageRunner Stories.baseCamp mirageCards 1 msg attrs
