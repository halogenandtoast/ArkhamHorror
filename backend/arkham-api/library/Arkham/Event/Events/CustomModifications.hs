module Arkham.Event.Events.CustomModifications (customModifications) where

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.Ref (targetToSource)
import Arkham.Helpers.SkillTest (
  getSkillTestInvestigator,
  getSkillTestSource,
  getSkillTestTarget,
  inAttackSkillTest,
  withSkillTest,
 )
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Trait (Trait (Upgrade))
import Arkham.Window (revealedChaosTokens)

newtype CustomModifications = CustomModifications EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customModifications :: EventCard CustomModifications
customModifications = event CustomModifications Cards.customModifications

instance HasModifiersFor CustomModifications where
  getModifiersFor (CustomModifications a) = do
    let
      usingAsset = do
        iid <- MaybeT getSkillTestInvestigator
        guard $ a.controller == iid
        attachedAsset <- MaybeT $ selectOne $ assetWithAttachedEvent a <> assetControlledBy a.controller
        guardM $ lift inAttackSkillTest
        source <- MaybeT getSkillTestSource
        guard $ source.asset == Just attachedAsset

    notchedSight <- runMaybeT do
      usingAsset
      guard $ a `hasCustomization` NotchedSight
      EnemyTarget eid <- MaybeT getSkillTestTarget
      guardM $ lift $ eid <=~> EnemyIsEngagedWith (not_ $ InvestigatorWithId a.controller)
      pure DoesNotDamageOtherInvestigator
    extendedStock <- runMaybeT do
      usingAsset
      guard $ a `hasCustomization` ExtendedStock
      pure $ SkillModifier #combat 2

    modified_ a a.controller $ maybeToList notchedSight <> maybeToList extendedStock
    modifiedWhen_
      a
      (a `hasCustomization` LeatherGrip)
      a.controller
      [ReduceCostOf (CardWithId a.cardId) 1]
    modifiedWhen_ a (a `hasCustomization` LeatherGrip) a.cardId [BecomesFast (DuringTurn You)]

instance HasAbilities CustomModifications where
  getAbilities (CustomModifications a) =
    case a.attachedTo of
      Just (AssetTarget aid) ->
        [ restricted
            a
            1
            ( DuringSkillTest
                $ #fighting
                <> SkillTestSourceMatches (SourceIsAsset (AssetControlledBy You <> AssetWithId aid))
            )
            $ ReactionAbility (RevealChaosToken #when You (not_ #autofail)) (exhaust a)
        ]
          <> [ restricted a 2 (ControlsThis <> can.draw.cards You)
                 $ CustomizationReaction
                   "Counterbalance"
                   ( AttachCard
                       #after
                       (Just You)
                       (CardWithTrait Upgrade <> not_ (CardWithId $ toCardId a))
                       (TargetIs $ AssetTarget aid)
                   )
                   Free
             | a `hasCustomization` Counterbalance
             ]
          <> [ restricted a 3 ControlsThis
                 $ CustomizationReaction
                   "Extended Magazine"
                   ( oneOf
                       [ SpentUses #after Anyone (SourceIsEvent $ not_ $ EventWithId a.id) Ammo (AssetWithId aid) AnyValue
                       , PlacedToken #after (SourceIsEvent $ not_ $ EventWithId a.id) (TargetIs $ AssetTarget aid) Ammo
                       ]
                   )
                   Free
             | a `hasCustomization` ExtendedMagazine
             ]
      _ -> []

instance RunMessage CustomModifications where
  runMessage msg e@(CustomModifications attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <-
        getUpgradeTargets iid
          $ assetControlledBy iid
          <> #firearm
          <> not_ (AssetWithAttachedEvent $ eventIs Cards.customModifications)
      chooseOne
        iid
        [ targetLabel asset [PlaceEvent attrs.id (AttachedToAsset asset Nothing)]
        | asset <- assets
        ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 (revealedChaosTokens -> [token]) _ -> do
      let source = toAbilitySource attrs 1
      cancelChaosToken source iid token
      pushAll
        [ ReturnChaosTokens [token]
        , UnfocusChaosTokens
        , DrawAnotherChaosToken iid
        ]
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawCardsIfCan iid (attrs.ability 2) 1
      pure e
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      for_ attrs.attachedTo \t -> placeTokens (attrs.ability 3) t Ammo 1
      pure e
    When (PassedThisSkillTestBy iid (AbilitySource source _) n)
      | maybe False ((`isSource` source) . targetToSource) attrs.attachedTo
          && (n >= 3)
          && (attrs `hasCustomization` QuicksilverBullets) -> do
          whenM inAttackSkillTest do
            withSkillTest \sid -> skillTestModifier sid (attrs.ability 3) iid (DamageDealt 1)
          pure e
    _ -> CustomModifications <$> liftRunMessage msg attrs
