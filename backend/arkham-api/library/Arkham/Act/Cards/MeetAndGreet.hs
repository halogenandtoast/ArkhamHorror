module Arkham.Act.Cards.MeetAndGreet (meetAndGreet) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillType
import Arkham.SlotType
import Arkham.Trait
import Data.List.Extra (nubOrd)

newtype MeetAndGreet = MeetAndGreet ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meetAndGreet :: ActCard MeetAndGreet
meetAndGreet = act (1, A) MeetAndGreet Cards.meetAndGreet $ groupClueCost (PerPlayer 3)

instance HasModifiersFor MeetAndGreet where
  getModifiersFor (MeetAndGreet a) =
    modifySelect a (AssetWithTrait Guest) [DoNotTakeUpSlot AllySlot]

instance HasAbilities MeetAndGreet where
  getAbilities = actAbilities \a ->
    [ skillTestAbility
        $ restricted
          a
          1
          (exists $ AssetWithTrait Guest <> AssetAt YourLocation <> not_ (AssetControlledBy You))
          parleyAction_
    , restricted
        a
        2
        ( exists (AssetWithTrait Guest <> AssetControlledBy You)
            <> exists (affectsOthers $ not_ You <> InvestigatorAt YourLocation)
        )
        actionAbility
    , restricted
        a
        3
        ( EachUndefeatedInvestigator (ControlsAsset $ AssetWithTrait Guest)
            <> exists (mapOneOf (AgendaWithSequence . (`AS.Sequence` AS.A)) [2, 3])
        )
        $ Objective
        $ FastAbility (GroupClueCost (PerPlayer 3) Anywhere)
    ]

instance RunMessage MeetAndGreet where
  runMessage msg a@(MeetAndGreet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ AssetWithTrait Guest <> AssetAt (locationWithInvestigator iid)
      chooseTargetM iid assets (handleTarget iid (attrs.ability 1))
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      card <- fetchCard aid
      let
        skillKinds = nubOrd $ flip mapMaybe card.icons \case
          SkillIcon kind -> Just kind
          _ -> Nothing
      sid <- getRandom
      chooseOneM iid do
        for_ skillKinds \kind -> do
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 1) aid kind (Fixed 3)

      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= traverse_ \case
        AssetTarget aid -> takeControlOfAsset iid aid
        _ -> error "Wrong target"
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assets <- select $ AssetWithTrait Guest <> assetControlledBy iid
      investigators <- select $ affectsOthers (colocatedWith iid) <> not_ (InvestigatorWithId iid)
      chooseTargetM iid assets \aid -> do
        chooseTargetM iid investigators (`takeControlOfAsset` aid)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      secondFloor <-
        placeRandomLocationGroupCapture "secondFloor"
          =<< shuffle
          =<< getSetAsideCardsMatching (CardWithTrait SecondFloor)
      case secondFloor of
        (x : _) -> do
          lobby <- selectJust $ locationIs Locations.lobbyTheMidwinterGala
          connectBothWays lobby x
        _ -> error "no second floor locations set aside"

      push $ ScenarioSpecific "placeRival" Null
      push $ ScenarioSpecific "readInterlude" Null
      advanceActDeck attrs
      pure a
    _ -> MeetAndGreet <$> liftRunMessage msg attrs
