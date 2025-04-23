module Arkham.Asset.Assets.ProfessorWilliamWebbFinderOfHiddenConnections (
  professorWilliamWebbFinderOfHiddenConnections,
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message.Lifted.Choose
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
    [ restricted a 1 ControlsThis
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
        withSkillTest \sid ->
          skillTestModifier
            sid
            (attrs.ability 1)
            (toTarget lid)
            (AlternateSuccessfullInvestigation (toTarget attrs))

      chooseOneM iid do
        when (hasItems && canGetItem) do
          labeled "Return item to your hand" $ doStep 1 msg
        when hasLocations do
          labeled "Discover a clue at a connecting location" $ doStep 2 msg

      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      items <- select $ #item <> inDiscardOf iid
      focusCards items $ chooseTargetM iid items $ addToHand iid . only
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      locations <-
        select $ ConnectedFrom (locationWithInvestigator iid) <> locationWithDiscoverableCluesBy iid
      chooseTargetM iid locations \lid -> discoverAt NotInvestigate iid (attrs.ability 1) lid 1
      pure a
    _ -> ProfessorWilliamWebbFinderOfHiddenConnections <$> liftRunMessage msg attrs
