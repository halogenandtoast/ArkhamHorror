module Arkham.Event.Cards.CustomModifications (customModifications, CustomModifications (..)) where

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (RevealChaosToken)
import Arkham.Game.Helpers (cancelChaosToken, targetToSource)
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.SkillTest (
  getSkillTestInvestigator,
  getSkillTestSource,
  getSkillTestTarget,
  inAttackSkillTest,
 )
import Arkham.Matcher
import Arkham.Message.Type
import Arkham.Placement
import Arkham.Trait (Trait (Upgrade))
import Arkham.Window (revealedChaosTokens)

-- □□□□ Quicksilver Bullets. If you succeed by 3 or more while attacking with
-- attached asset, this attack deals +1 damage.

newtype CustomModifications = CustomModifications EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customModifications :: EventCard CustomModifications
customModifications = event CustomModifications Cards.customModifications

instance HasModifiersFor CustomModifications where
  getModifiersFor (InvestigatorTarget iid) (CustomModifications a) = do
    let
      usingAsset = do
        iid' <- MaybeT $ getSkillTestInvestigator
        guard $ iid == iid'
        attachedAsset <- MaybeT $ selectOne $ assetWithAttachedEvent a <> assetControlledBy iid
        guardM $ lift inAttackSkillTest
        source <- MaybeT getSkillTestSource
        guard $ source.asset == Just attachedAsset

    notchedSight <- runMaybeT do
      usingAsset
      guard $ a `hasCustomization` NotchedSight
      EnemyTarget eid <- MaybeT getSkillTestTarget
      guardM $ lift $ eid <=~> EnemyIsEngagedWith (not_ $ InvestigatorWithId iid)
      pure DoesNotDamageOtherInvestigator
    extendedStock <- runMaybeT do
      usingAsset
      guard $ a `hasCustomization` ExtendedStock
      pure $ SkillModifier #combat 2

    modified a $ maybeToList notchedSight <> maybeToList extendedStock
  getModifiersFor target (CustomModifications a) | isTarget a target = do
    modified a
      $ guard (a `hasCustomization` LeatherGrip)
      *> [ReduceCostOf (CardWithId a.cardId) 1, BecomesFast (DuringTurn You)]
  getModifiersFor _ _ = pure []

instance HasAbilities CustomModifications where
  getAbilities (CustomModifications a) =
    case a.attachedTo of
      Just (AssetTarget aid) ->
        [ restrictedAbility
            a
            1
            ( DuringSkillTest
                $ #fighting
                <> SkillTestSourceMatches (SourceIsAsset (AssetControlledBy You <> AssetWithId aid))
            )
            $ ReactionAbility (RevealChaosToken #when You (not_ #autofail)) (exhaust a)
        ]
          <> [ restrictedAbility a 2 (ControlsThis <> can.draw.cards You)
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
          <> [ restrictedAbility a 3 ControlsThis
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
        select
          $ assetControlledBy iid
          <> #firearm
          <> not_ (AssetWithAttachedEvent $ eventIs Cards.customModifications)
      chooseOne
        iid
        [ targetLabel asset [PlaceEvent iid attrs.id (AttachedToAsset asset Nothing)]
        | asset <- assets
        ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 (revealedChaosTokens -> [token]) _ -> do
      let source = toAbilitySource attrs 1
      cancelChaosToken token
      pushAll
        [ CancelEachNext source [RunWindowMessage, DrawChaosTokenMessage, RevealChaosTokenMessage]
        , ReturnChaosTokens [token]
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
    When (PassedThisSkillTestBy iid (AbilitySource source _) n) | maybe False ((`isSource` source) . targetToSource) (attrs.attachedTo) && n >= 3 -> do
      whenM inAttackSkillTest do
        skillTestModifier (attrs.ability 3) iid (DamageDealt 1)
      pure e
    _ -> CustomModifications <$> liftRunMessage msg attrs
