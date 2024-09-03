module Arkham.Skill.Cards.Grizzled (grizzled, Grizzled (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyTraits))
import Arkham.Game.Helpers (sourceMatches)
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestSource, getSkillTestTarget)
import Arkham.Helpers.Source
import Arkham.Helpers.Target
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (EncounterCardSource)
import Arkham.SkillType
import Arkham.Treachery.Types (Field (TreacheryTraits))

newtype Grizzled = Grizzled SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor Grizzled where
  getModifiersFor target (Grizzled a) | isTarget a target = do
    let traits = [t | ChosenTrait t <- concatMap snd (toList a.customizations)]
    maybeModified a do
      guard $ isNothing a.attachedTo
      s <- MaybeT $ getSkillTestSource
      t <- MaybeT $ getSkillTestTarget
      isEncounterCardTarget <- lift $ targetMatches t ScenarioCardTarget
      isEncounterCardSource <- lift $ sourceMatches s EncounterCardSource
      n <-
        lift
          $ if isEncounterCardTarget
            then do
              traits' <- targetTraits t
              pure $ count (`member` traits') traits
            else
              if isEncounterCardSource
                then do
                  traits' <- sourceTraits s
                  pure $ count (`member` traits') traits
                else pure 0
      guard $ n > 0
      pure [AddSkillIcons $ concat $ replicate @[[SkillIcon]] n [WildIcon, WildIcon]]
  getModifiersFor (SkillTestTarget _) (Grizzled a) = case a.attachedTo of
    Just (EnemyTarget eid) -> do
      maybeModified a do
        source <- MaybeT $ getSkillTestSource
        target <- MaybeT $ getSkillTestTarget
        let isOnEnemy = maybe False (== eid) source.enemy
        let isAgainstEnemy = maybe False (== eid) target.enemy
        guard $ isOnEnemy || isAgainstEnemy
        pure [Difficulty (-1)]
    _ -> pure []
  getModifiersFor _ _ = pure []

grizzled :: SkillCard Grizzled
grizzled = skill Grizzled Cards.grizzled

instance HasAbilities Grizzled where
  getAbilities (Grizzled a) =
    [ limitedAbility (MaxPer Cards.grizzled PerRound 1)
      $ restrictedAbility
        a
        1
        InYourDiscard
        ( CustomizationReaction
            "Always Prepared"
            (DrawCard #after You (basic $ IsEncounterCard <> oneOf (CardWithTrait <$> traits)) AnyDeck)
            Free
        )
    | a `hasCustomization` AlwaysPrepared
    ]
   where
    traits = [t | ChosenTrait t <- concatMap snd (toList a.customizations)]

instance RunMessage Grizzled where
  runMessage msg s@(Grizzled attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      let traits = [t | ChosenTrait t <- concatMap snd (toList attrs.customizations)]
      getSkillTestTarget >>= traverse_ \case
        EnemyTarget eid -> do
          when (attrs `hasCustomization` Nemesis) do
            enemyTraits <- field EnemyTraits eid
            when (any (`elem` traits) enemyTraits) do
              chooseOne
                iid
                [ Label "Attach to enemy (Nemesis)" [PlaceSkill attrs.id $ AttachedToEnemy eid]
                , Label "Do not attach to enemy" []
                ]
        TreacheryTarget tid -> do
          when (attrs `hasCustomization` MythosHardened) do
            treacheryTraits <- field TreacheryTraits tid
            when (any (`elem` traits) treacheryTraits) do
              chooseOne
                iid
                [ Label
                    "Add both the treachery and Grizzled to the victory display (Mythos-Hardened)"
                    [AddToVictory (toTarget attrs), AddToVictory (toTarget tid)]
                , Label "Do not add to victory" []
                ]
        _ -> pure ()
      pure s
    InDiscard _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ AddToHand iid [toCard attrs]
      pure s
    _ -> Grizzled <$> liftRunMessage msg attrs
