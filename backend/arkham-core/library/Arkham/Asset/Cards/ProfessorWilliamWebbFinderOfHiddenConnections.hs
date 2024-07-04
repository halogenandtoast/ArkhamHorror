module Arkham.Asset.Cards.ProfessorWilliamWebbFinderOfHiddenConnections (
  professorWilliamWebbFinderOfHiddenConnections,
  ProfessorWilliamWebbFinderOfHiddenConnections (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Discover
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Modifier

newtype ProfessorWilliamWebbFinderOfHiddenConnections
  = ProfessorWilliamWebbFinderOfHiddenConnections AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWilliamWebbFinderOfHiddenConnections
  :: AssetCard ProfessorWilliamWebbFinderOfHiddenConnections
professorWilliamWebbFinderOfHiddenConnections =
  ally
    ProfessorWilliamWebbFinderOfHiddenConnections
    Cards.professorWilliamWebbFinderOfHiddenConnections
    (1, 2)

instance HasAbilities ProfessorWilliamWebbFinderOfHiddenConnections where
  getAbilities (ProfessorWilliamWebbFinderOfHiddenConnections a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (SuccessfulInvestigation #when You Anywhere) (exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage ProfessorWilliamWebbFinderOfHiddenConnections where
  runMessage msg a@(ProfessorWilliamWebbFinderOfHiddenConnections attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasItems <- selectAny $ #item <> inDiscardOf iid
      canGetItem <- can.have.cards.leaveDiscard iid
      hasLocations <-
        selectAny $ ConnectedFrom (locationWithInvestigator iid) <> locationWithDiscoverableCluesBy iid

      -- We redirect success to this, but do nothing since the ability handles it
      withLocationOf iid \lid ->
        skillTestModifier
          (attrs.ability 1)
          (toTarget lid)
          (AlternateSuccessfullInvestigation (toTarget attrs))

      chooseOne
        iid
        $ [Label "Return item to your hand" [DoStep 1 msg] | hasItems && canGetItem]
        <> [Label "Discover a clue at a connecting location" [DoStep 2 msg] | hasLocations]

      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      items <- select $ #item <> inDiscardOf iid
      focusCards items \unfocus -> do
        chooseOne iid [targetLabel item [unfocus, AddToHand iid [item]] | item <- items]
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      locations <-
        select $ ConnectedFrom (locationWithInvestigator iid) <> locationWithDiscoverableCluesBy iid
      chooseOne
        iid
        [ targetLabel location [DiscoverClues iid $ discover location (attrs.ability 1) 1]
        | location <- locations
        ]
      pure a
    _ -> ProfessorWilliamWebbFinderOfHiddenConnections <$> liftRunMessage msg attrs
