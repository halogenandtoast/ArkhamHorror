module Arkham.Asset.Cards.CloseTheCircle1 (closeTheCircle1, CloseTheCircle1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Game.Helpers (canDo, getCanPerformAbility, getPlayableCards)
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Message (drawCards)
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Investigator)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype Metadata = Metadata {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CloseTheCircle1 = CloseTheCircle1 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeTheCircle1 :: AssetCard CloseTheCircle1
closeTheCircle1 = asset (CloseTheCircle1 . (`with` Metadata False)) Cards.closeTheCircle1

instance HasModifiersFor CloseTheCircle1 where
  getModifiersFor (InvestigatorTarget iid) (CloseTheCircle1 (With a meta)) | iid `controls` a = do
    pure
      $ toModifiers a
      $ guard (active meta)
      *> [UseSkillInPlaceOf sType #willpower | sType <- [#combat, #agility, #intellect]]
  getModifiersFor _ _ = pure []

instance HasAbilities CloseTheCircle1 where
  getAbilities (CloseTheCircle1 (With a _)) =
    [ controlledAbility a 1 (exists $ BasicAbility <> PerformableAbility [ActionCostModifier (-1)])
        $ FastAbility (assetUseCost a Charge 1 <> exhaust a)
    ]

instance RunMessage CloseTheCircle1 where
  runMessage msg (CloseTheCircle1 (With attrs meta)) = runQueueT $ case msg of
    InvestigatorPlayedAsset iid aid | aid == toId attrs -> do
      cards <- select $ ControlledBy (InvestigatorWithId iid)

      let n =
            length
              $ filter (`notElem` [Neutral, Mythos])
              . nub
              . (Mystic :)
              $ concatMap (toList . cdClassSymbols . toCardDef . toCard) cards
      CloseTheCircle1
        . (`with` meta)
        <$> runMessage
          msg
          (attrs {assetPrintedUses = Uses Charge (Static n), assetUses = singletonMap Charge n})
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let windows = defaultWindows iid
      iattrs <- getAttrs @Investigator iid
      withModifiers iid (toModifiers attrs [ActionCostModifier (-1)]) $ do
        abilities <- filterM (getCanPerformAbility iid (defaultWindows iid)) =<< select BasicAbility
        canDraw <- canDo iid #draw
        playableCards <-
          filter (`cardMatch` CardWithoutAction) <$> getPlayableCards iattrs (UnpaidCost NoAction) windows
        canTakeResource <- (&&) <$> canDo iid #resource <*> can.gain.resources FromOtherSource iid
        canAffordTakeResources <- getCanAfford iattrs [#resource]
        drawing <- lift $ drawCards iid iid 1
        canAffordDrawCards <- getCanAfford iattrs [#draw]
        canPlay <- canDo iid #play
        mods <- getModifiers iid
        lift
          $ chooseOne iid
          $ [ ResourceLabel iid [TakeResources iid 1 (toSource iid) False]
            | canAffordTakeResources && canTakeResource
            ]
          <> [ ComponentLabel (InvestigatorDeckComponent iid) [drawing]
             | canAffordDrawCards
             , canDraw
             , none (`elem` mods) [CannotDrawCards, CannotManipulateDeck]
             ]
          <> [ targetLabel (toCardId c) [InitiatePlayCard iid c Nothing NoPayment windows False]
             | canPlay
             , c <- playableCards
             ]
          <> map ((\f -> f windows []) . AbilityLabel iid) abilities
        lift $ push $ DoStep 1 msg
        pure . CloseTheCircle1 $ attrs `with` Metadata True
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      pure . CloseTheCircle1 $ attrs `with` Metadata False
    _ -> CloseTheCircle1 . (`with` meta) <$> lift (runMessage msg attrs)
