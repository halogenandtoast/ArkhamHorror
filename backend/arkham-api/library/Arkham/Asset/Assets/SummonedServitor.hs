module Arkham.Asset.Assets.SummonedServitor (summonedServitor) where

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Customization
import Arkham.Helpers.Location (onSameLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelfWhen)
import Arkham.Helpers.SkillTest.Lifted (investigateLocation_)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Projection

newtype SummonedServitor = SummonedServitor AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

summonedServitor :: AssetCard SummonedServitor
summonedServitor = asset SummonedServitor Cards.summonedServitor

instance HasModifiersFor SummonedServitor where
  getModifiersFor (SummonedServitor a) = do
    let indexes = [i | ChosenIndex i <- concatMap snd (toList a.customizations)]
    modifySelfWhen
      a
      (a `hasCustomization` Dominance)
      [DoNotTakeUpSlot $ if 0 `elem` indexes then #arcane else #ally]
    modifySelectWhen
      a
      (a `hasCustomization` ArmoredCarapace)
      (InvestigatorAt $ locationWithAsset a)
      [CanAssignDamageToAsset a.id]

instance HasAbilities SummonedServitor where
  getAbilities (SummonedServitor a) =
    guard (length used < if a `hasCustomization` DæmonicInfluence then 2 else 1)
      *> [ controlled
             a
             1
             (exists $ RevealedLocation <> ConnectedFrom (locationWithAsset a.id))
             $ ServitorAbility #move
         | #move `notElem` used
         ]
      <> [ controlled a 2 (exists $ EnemyAt $ locationWithAsset a.id) $ ServitorAbility #fight
         | a `hasCustomization` ClawsThatCatch
         , #fight `notElem` used
         ]
      <> [ controlled a 3 (exists $ EnemyAt $ locationWithAsset a.id) $ ServitorAbility #evade
         | a `hasCustomization` JawsThatSnatch
         , #evade `notElem` used
         ]
      <> [ controlled a 4 (exists $ InvestigatableLocation <> locationWithAsset a.id)
             $ ServitorAbility #investigate
         | a `hasCustomization` EyesOfFlame
         , #investigate `notElem` used
         ]
   where
    used = getAssetMetaDefault @[Action] [] a

instance RunMessage SummonedServitor where
  runMessage msg a@(SummonedServitor attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      mowner <- field AssetOwner (toId attrs)
      mlid <- maybe (pure Nothing) (field InvestigatorLocation) mowner
      case mlid of
        Nothing -> toDiscardBy iid attrs attrs
        Just lid -> push $ PlaceAsset attrs.id (AtLocation lid)
      pure
        $ if attrs `hasCustomization` ArmoredCarapace
          then SummonedServitor $ attrs & healthL ?~ 3
          else a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ RevealedLocation <> ConnectedFrom (locationWithAsset attrs.id)
      when (notNull locations) do
        onSame <- onSameLocation iid attrs.placement
        locationsCanEnter <-
          select
            $ RevealedLocation
            <> AccessibleFrom (locationWithAsset attrs.id)
            <> CanEnterLocation (InvestigatorWithId iid)
        chooseTargetM iid locations \lid -> do
          place attrs (AtLocation lid)
          when (attrs `hasCustomization` WingsOfNight && onSame && lid `elem` locationsCanEnter) do
            handleTarget iid attrs lid
      pure . SummonedServitor $ overMeta (<>) [Action.Move] attrs
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      chooseOneM iid do
        labeled "Move to location with Summoned Servitor (Wings of Night)"
          $ moveTo (attrs.ability 1) iid lid
        labeled "Do not move" nothing
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 2) iid [BaseSkillOf #combat 4, IgnoreAloof, IgnoreRetaliate]
      chooseFightEnemyMatch
        sid
        iid
        (attrs.ability 2)
        (fightOverride $ EnemyAt $ locationWithAsset attrs.id)
      pure . SummonedServitor $ overMeta (<>) [Action.Fight] attrs
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 2) iid [BaseSkillOf #agility 4, IgnoreAlert]
      chooseEvadeEnemyMatch
        sid
        iid
        (attrs.ability 2)
        (evadeOverride $ EnemyAt (locationWithAsset attrs.id) <> EnemyCanBeEvadedBy (attrs.ability 3))
      pure . SummonedServitor $ overMeta (<>) [Action.Evade] attrs
    UseThisAbility iid (isSource attrs -> True) 4 -> do
      lid <- selectJust $ locationWithAsset attrs.id
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 4) iid [BaseSkillOf #intellect 4]
      investigateLocation_ sid iid (attrs.ability 4) lid
      pure . SummonedServitor $ overMeta (<>) [Action.Investigate] attrs
    BeginTurn iid | attrs `controlledBy` iid -> do
      pure . SummonedServitor $ setMeta @[Action] [] attrs
    _ -> SummonedServitor <$> liftRunMessage msg attrs
