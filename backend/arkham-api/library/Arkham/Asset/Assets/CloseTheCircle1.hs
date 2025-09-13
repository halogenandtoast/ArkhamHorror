module Arkham.Asset.Assets.CloseTheCircle1 (closeTheCircle1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Action (canDo, getCanAfford)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Investigator.Types (Investigator, remainingActionsL)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype Metadata = Metadata {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CloseTheCircle1 = CloseTheCircle1 (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeTheCircle1 :: AssetCard CloseTheCircle1
closeTheCircle1 = asset (CloseTheCircle1 . (`with` Metadata False)) Cards.closeTheCircle1

instance HasModifiersFor CloseTheCircle1 where
  getModifiersFor (CloseTheCircle1 (With a meta)) = for_ a.controller \iid -> do
    let mods = map (`UseSkillInPlaceOf` #willpower) [#combat, #agility, #intellect]
    modifiedWhen_ a (active meta) iid mods

instance HasAbilities CloseTheCircle1 where
  getAbilities (CloseTheCircle1 (With a _)) =
    [ controlled a 1 (exists $ #basic <> PerformableAbility [#noAction])
        $ FastAbility (assetUseCost a Charge 1 <> exhaust a)
    ]

data CloseTheCircle1State = CloseTheCircle1State
  { canTakeResource :: Bool
  , canDraw :: Bool
  , playableCards :: [Card]
  }

instance RunMessage CloseTheCircle1 where
  runMessage msg (CloseTheCircle1 (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let windows = defaultWindows iid
      iattrs <- getAttrs @Investigator iid
      abilities <- selectMap (`decrease_` 1) (performableAbilityWithoutActionBy iid #basic)
      cstate <- withGrantedAction iid attrs do
        canPlay <- canDo iid #play
        playableCards <-
          if canPlay
            then
              filterCards CardWithoutAction
                <$> getPlayableCards (attrs.ability 1) iid (UnpaidCost NoAction) windows
            else pure []
        canTakeResource <-
          andM
            [ canDo iid #resource
            , can.gain.resources FromOtherSource iid
            , getCanAfford (iattrs & remainingActionsL +~ 1) [#resource]
            ]
        canDraw <-
          andM
            [ getCanAfford (iattrs & remainingActionsL +~ 1) [#draw]
            , canDo iid #draw
            ]
        pure CloseTheCircle1State {..}

      chooseOneM iid do
        when cstate.canTakeResource $ resourceLabeled iid $ gainResources iid iid 1
        when cstate.canDraw $ deckLabeled iid $ drawCards iid iid 1
        targets cstate.playableCards (playCardPayingCost iid)
        for_ abilities (abilityLabeled_ iid)
      doStep 1 msg
      pure . CloseTheCircle1 $ attrs `with` Metadata True
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      pure . CloseTheCircle1 $ attrs `with` Metadata False
    _ -> CloseTheCircle1 . (`with` meta) <$> liftRunMessage msg attrs
