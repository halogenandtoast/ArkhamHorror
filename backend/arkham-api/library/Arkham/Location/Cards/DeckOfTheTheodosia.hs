module Arkham.Location.Cards.DeckOfTheTheodosia (deckOfTheTheodosia) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype DeckOfTheTheodosia = DeckOfTheTheodosia LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deckOfTheTheodosia :: LocationCard DeckOfTheTheodosia
deckOfTheTheodosia = location DeckOfTheTheodosia Cards.deckOfTheTheodosia 1 (PerPlayer 3)

mirageCards :: [CardDef]
mirageCards = [Cards.coastalWaters, Cards.hedgeMaze, Cards.standingStones]

instance HasModifiersFor DeckOfTheTheodosia where
  getModifiersFor (DeckOfTheTheodosia a) = clearedOfMirages a mirageCards

instance HasAbilities DeckOfTheTheodosia where
  getAbilities (DeckOfTheTheodosia a) =
    extendRevealed
      a
      [ mirage a 1 mirageCards
      , mkAbility a 1
          $ forced
          $ InitiatedSkillTest #when You AnySkillType AnySkillTestValue
          $ WhileInvestigating (be a)
      ]

instance RunMessage DeckOfTheTheodosia where
  runMessage msg l@(DeckOfTheTheodosia attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      actions <- field InvestigatorRemainingActions iid
      chooseOneM iid do
        labeled "Give it +2 shroud for this investigation" do
          withSkillTest \sid ->
            skillTestModifier sid (attrs.ability 2) attrs (ShroudModifier 2)
        when (actions > 0) do
          labeled "Spend 1 additional action" $ loseActions iid (attrs.ability 2) 1
      pure l
    _ -> DeckOfTheTheodosia <$> mirageRunner Stories.deckOfTheTheodosia mirageCards 1 msg attrs
