module Arkham.Asset.Assets.ProfessorWilliamWebbFinderOfHiddenConnections2 (
  professorWilliamWebbFinderOfHiddenConnections2,
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

newtype ProfessorWilliamWebbFinderOfHiddenConnections2
  = ProfessorWilliamWebbFinderOfHiddenConnections2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWilliamWebbFinderOfHiddenConnections2
  :: AssetCard ProfessorWilliamWebbFinderOfHiddenConnections2
professorWilliamWebbFinderOfHiddenConnections2 =
  ally
    ProfessorWilliamWebbFinderOfHiddenConnections2
    Cards.professorWilliamWebbFinderOfHiddenConnections2
    (1, 3)

instance HasAbilities ProfessorWilliamWebbFinderOfHiddenConnections2 where
  getAbilities (ProfessorWilliamWebbFinderOfHiddenConnections2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (SuccessfulInvestigation #when You Anywhere) (exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage ProfessorWilliamWebbFinderOfHiddenConnections2 where
  runMessage msg a@(ProfessorWilliamWebbFinderOfHiddenConnections2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenM (can.have.cards.leaveDiscard iid) do
        items <- select $ #item <> inDiscardOf iid
        focusCards items $ chooseTargetM iid items $ addToHand iid . only

      hasLocations <-
        selectAny $ ConnectedFrom (locationWithInvestigator iid) <> locationWithDiscoverableCluesBy iid

      when hasLocations do
        chooseOneM iid do
          labeled "Discover clue at your location" nothing
          labeled "Discover a clue at a connecting location" $ doStep 1 msg

      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      -- We redirect success to this, but do nothing since the ability handles it
      withLocationOf iid \lid ->
        withSkillTest \sid ->
          skillTestModifier sid (attrs.ability 1) lid
            $ AlternateSuccessfullInvestigation (toTarget attrs)
      locations <-
        select $ ConnectedFrom (locationWithInvestigator iid) <> locationWithDiscoverableCluesBy iid
      chooseTargetM iid locations \lid -> discoverAt NotInvestigate iid (attrs.ability 1) lid 1
      pure a
    _ -> ProfessorWilliamWebbFinderOfHiddenConnections2 <$> liftRunMessage msg attrs
