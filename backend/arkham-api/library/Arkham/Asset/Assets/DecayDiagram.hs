module Arkham.Asset.Assets.DecayDiagram (decayDiagram) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers (scenarioI18n)

newtype DecayDiagram = DecayDiagram AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decayDiagram :: AssetCard DecayDiagram
decayDiagram = asset DecayDiagram Cards.decayDiagram

-- While you are in the Chamber of Decay, it gains: "[action]: Place 1 doom
-- here. Flip any number of your clues to their doom side and place them
-- here."
instance HasAbilities DecayDiagram where
  getAbilities (DecayDiagram a) =
    [ restricted a 1 (ControlsThis <> exists (NotInvestigator You))
        $ forced
        $ Matcher.InvestigatorDefeated #when ByAny You
    , restricted
        a
        2
        (ControlsThis <> youExist (InvestigatorAt $ locationIs Locations.chamberOfDecay))
        actionAbility
    ]

instance RunMessage DecayDiagram where
  runMessage msg a@(DecayDiagram attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ NotInvestigator $ InvestigatorWithId iid
      chooseOrRunOneM iid do
        targets investigators (`takeControlOfAsset` attrs.id)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      placeTokens (attrs.ability 2) attrs #doom 1
      clues <- field InvestigatorClues iid
      when (clues > 0) do
        scenarioI18n $ chooseAmount' iid "decayDiagram.cluesToFlip" "$clues" 0 clues attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      removeTokens (attrs.ability 2) iid #clue n
      placeTokens (attrs.ability 2) attrs #doom n
      pure a
    _ -> DecayDiagram <$> liftRunMessage msg attrs
