{-# LANGUAGE MultiWayIf #-}

module Arkham.Event.Cards.TheRavenQuill (theRavenQuill, TheRavenQuill (..)) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Helpers.Customization
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified, modified)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, getSkillTestSource)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Strategy
import Arkham.Window (defaultWindows)

newtype TheRavenQuill = TheRavenQuill EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRavenQuill :: EventCard TheRavenQuill
theRavenQuill = event TheRavenQuill Cards.theRavenQuill

instance HasModifiersFor TheRavenQuill where
  getModifiersFor target (TheRavenQuill a) | a.attachedTo == Just target = do
    let
      spectralBinding = do
        guard $ a `hasCustomization` SpectralBinding
        DoNotTakeUpSlot <$> [minBound ..]
    modified a spectralBinding
  getModifiersFor (AbilityTarget _ ab) (TheRavenQuill a) | a.attachedTo == fmap AssetTarget ab.source.asset = do
    let
      livingQuill = do
        guard $ a `hasCustomization` LivingQuill
        ActionDoesNotCauseAttacksOfOpportunity <$> ab.actions
    modified a livingQuill
  getModifiersFor (InvestigatorTarget iid) (TheRavenQuill a) = do
    maybeModified a do
      guard $ a `hasCustomization` MysticVane
      source <- MaybeT getSkillTestSource
      guard $ a.attachedTo == fmap AssetTarget source.asset
      iid' <- MaybeT getSkillTestInvestigator
      guard $ iid == iid'
      pure [AnySkillValue 2]
  getModifiersFor _ _ = pure []

instance HasAbilities TheRavenQuill where
  getAbilities (TheRavenQuill a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ oneOf [GameEnds #when, InvestigatorResigned #when You]
    ]
      <> [ controlledAbility
          a
          2
          ( exists
              $ AssetControlledBy You
              <> oneOf (AssetWithUses <$> [Charge, Secret])
              <> not_ (assetWithAttachedEvent a.id)
          )
          $ FastAbility (exhaust a)
         | a `hasCustomization` EnergySap
         ]
      <> [ controlledAbility
          a
          3
          (exists $ AssetControlledBy You <> #exhausted <> not_ (assetWithAttachedEvent a.id))
          $ SilentForcedAbility (ActivateAbility #after You $ AbilityOnAsset $ assetWithAttachedEvent a.id)
         | a `hasCustomization` InterwovenInk && a.ready
         ]

instance RunMessage TheRavenQuill where
  runMessage msg e@(TheRavenQuill attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      -- Handles EndlessInkwell
      let titles = [t | ChosenCard t <- concatMap snd (toList attrs.customizations)]
      assets <- select $ assetControlledBy iid <> oneOf (AssetWithTitle <$> titles)

      -- you may search your deck, discard pile, and hand for a copy of a named asset
      if attrs `hasCustomization` SupernaturalRecord
        then do
          canSearch <- can.search.deck iid
          cards <-
            select
              $ PlayableCard (UnpaidCost NoAction)
              $ oneOf (inHandOf iid : inDiscardOf iid : [inDeckOf iid | canSearch])
              <> basic (oneOf $ CardWithTitle <$> titles)
          if null cards
            then
              chooseOne
                iid
                [ targetLabel asset [PlaceEvent iid attrs.id (AttachedToAsset asset Nothing), RefillSlots iid]
                | asset <- assets
                ]
            else do
              defaultChoose' <-
                if notNull assets
                  then
                    do evalQueueT
                      $ chooseOne
                        iid
                        [ targetLabel asset [PlaceEvent iid attrs.id (AttachedToAsset asset Nothing), RefillSlots iid]
                        | asset <- assets
                        ]
                  else pure []
              chooseOrRunOne
                iid
                $ Label
                  "Search your deck, discard pile, and hand for a copy of a named asset and play it (paying its cost). Then, attach The Raven Quill to it."
                  [DoStep 1 msg]
                : [Label "Do not search" defaultChoose' | notNull assets]
        else
          chooseOne
            iid
            [ targetLabel asset [PlaceEvent iid attrs.id (AttachedToAsset asset Nothing), RefillSlots iid]
            | asset <- assets
            ]

      pure e
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      let titles = [t | ChosenCard t <- concatMap snd (toList attrs.customizations)]
      canSearch <- can.search.deck iid
      search
        iid
        attrs
        iid
        ((FromHand, PutBack) : (FromDiscard, PutBack) : [(FromDeck, ShuffleBackIn) | canSearch])
        (PlayableCard (UnpaidCost NoAction) $ basic $ oneOf $ CardWithTitle <$> titles)
        (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      for_ cards \card ->
        cardResolutionModifiers attrs attrs card $ DoNotTakeUpSlot <$> [minBound ..]
      chooseOne
        iid
        [ targetLabel
          card
          [ Msg.addToHand iid card
          , PayCardCost iid card (defaultWindows iid)
          , handleTargetChoice iid attrs card
          ]
        | card <- cards
        ]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      selectOne (AssetWithCardId cid)
        >>= traverse_ (push . PlaceEvent iid attrs.id . (`AttachedToAsset` Nothing))
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- [DECKBUILDING]
      chooseOne
        iid
        [ Label
            "Either mark a checkbox on The Raven Quill's upgrade sheet, or reduce the experience cost to upgrade the attached asset before the next scenario by 1. (NOTE: you must handle this update manually when upgrading your deck via ArkhamDB)"
            []
        ]
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      for_ attrs.attachedTo \case
        AssetTarget aid -> do
          others <-
            select
              $ assetControlledBy iid
              <> oneOf (AssetWithUses <$> [Charge, Secret])
              <> not_ (AssetWithId aid)
          chooseOne iid $ targetLabels others $ only . handleTargetChoice iid (attrs.ability 2)
        _ -> error "invalid attach"
      pure e
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (AssetTarget sourceAsset) -> do
      for_ attrs.attachedTo \target -> do
        let moveToken tType = MoveTokens (attrs.ability 2) (toSource sourceAsset) target tType 1
        hasSecret <- sourceAsset <=~> AssetWithUses Secret
        hasCharge <- sourceAsset <=~> AssetWithUses Charge
        if
          | hasSecret && hasCharge ->
              chooseOne iid [Label "Move secret" [moveToken Secret], Label "Move charge" [moveToken Charge]]
          | hasSecret -> push $ moveToken Secret
          | hasCharge -> push $ moveToken Charge
          | otherwise -> error "invalid choice"
      pure e
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      for_ attrs.attachedTo \case
        AssetTarget aid -> do
          others <- select $ assetControlledBy iid <> #exhausted <> not_ (AssetWithId aid)
          chooseOne iid $ Label "Do not ready asset" []
            : [targetLabel asset [Exhaust (toTarget attrs), Ready (toTarget asset)] | asset <- others]
        _ -> error "invalid attach"
      pure e
    _ -> TheRavenQuill <$> liftRunMessage msg attrs
