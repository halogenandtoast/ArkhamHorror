module Arkham.Asset.Assets.DecayDiagram (decayDiagram) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype DecayDiagram = DecayDiagram AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decayDiagram :: AssetCard DecayDiagram
decayDiagram = asset DecayDiagram Cards.decayDiagram

instance HasAbilities DecayDiagram where
  getAbilities (DecayDiagram a) =
    [ controlled a 1 (exists $ not_ You) $ forced $ Matcher.InvestigatorDefeated #when ByAny You
    , restricted
        (proxied (locationIs Locations.chamberOfDecay) a)
        2
        (Here <> exists (at_ (locationIs Locations.chamberOfDecay) <> HasMatchingAsset (be a)))
        actionAbility
    ]

instance RunMessage DecayDiagram where
  runMessage msg a@(DecayDiagram attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ not_ $ InvestigatorWithId iid
      chooseOrRunOneM iid $ targets investigators (`takeControlOfAsset` attrs.id)
      pure a
    UseThisAbility iid (isProxySource attrs -> True) 2 -> do
      chamber <- selectJust $ locationIs Locations.chamberOfDecay
      placeTokens (attrs.ability 2) chamber #doom 1
      clues <- field InvestigatorClues iid
      when (clues > 0) $ withI18n $ chooseAmount' iid "clues" "$clues" 0 clues attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      chamber <- selectJust $ locationIs Locations.chamberOfDecay
      removeTokens (attrs.ability 2) iid #clue n
      placeTokens (attrs.ability 2) chamber #doom n
      pure a
    _ -> DecayDiagram <$> liftRunMessage msg attrs
