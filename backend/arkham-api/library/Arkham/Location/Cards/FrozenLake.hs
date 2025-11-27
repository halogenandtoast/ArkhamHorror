module Arkham.Location.Cards.FrozenLake (frozenLake) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FrozenLake = FrozenLake LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenLake :: LocationCard FrozenLake
frozenLake = location FrozenLake Cards.frozenLake 3 (PerPlayer 2)

instance HasAbilities FrozenLake where
  getAbilities (FrozenLake a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> DuringAnySkillTest)
      $ forced
      $ RevealChaosToken #after You
      $ oneOf [#skull, #cultist, #tablet, #elderthing]

instance RunMessage FrozenLake where
  runMessage msg l@(FrozenLake attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasCardsInHand <- selectAny $ inHandOf NotForPlay iid <> basic DiscardableCard
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
        labeledValidate' hasCardsInHand "discardCardsFromHand" $ chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> FrozenLake <$> liftRunMessage msg attrs
