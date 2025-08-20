module Arkham.Skill.Cards.Grizzled (grizzled) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (EnemyTraits))
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestSource, getSkillTestTarget)
import Arkham.Helpers.Source
import Arkham.Helpers.Target
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (EncounterCardSource)
import Arkham.SkillType
import Arkham.Treachery.Types (Field (TreacheryTraits))

newtype Grizzled = Grizzled SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor Grizzled where
  getModifiersFor (Grizzled a) = do
    let traits = [t | ChosenTrait t <- concatMap snd (toList a.customizations)]
    modifySelfMaybe a.cardId do
      guard $ isNothing a.attachedTo
      s <- MaybeT getSkillTestSource
      t <- MaybeT getSkillTestTarget
      n <- lift do
        isEncounterCardTarget <- targetMatches t ScenarioCardTarget
        if isEncounterCardTarget
          then do
            traits' <- targetTraits t
            pure $ count (`member` traits') traits
          else do
            isEncounterCardSource <- sourceMatches s EncounterCardSource
            if isEncounterCardSource
              then do
                traits' <- sourceTraits s
                pure $ count (`member` traits') traits
              else pure 0
      guard $ n > 0
      pure [AddSkillIcons $ concat $ replicate @[[SkillIcon]] n [#wild, #wild]]
    whenJustM getSkillTest \st -> do
      for_ a.attachedTo.enemy \eid ->
        maybeModified_ a (SkillTestTarget st.id) do
          source <- MaybeT $ getSkillTestSource
          target <- MaybeT $ getSkillTestTarget
          let isOnEnemy = maybe False (== eid) source.enemy
          let isAgainstEnemy = maybe False (== eid) target.enemy
          guard $ isOnEnemy || isAgainstEnemy
          pure [Difficulty (-1)]

grizzled :: SkillCard Grizzled
grizzled = skill Grizzled Cards.grizzled

instance HasAbilities Grizzled where
  getAbilities (Grizzled a) =
    [ limitedAbility (MaxPer Cards.grizzled PerRound 1)
        $ restricted a 1 InYourDiscard
        $ CustomizationReaction
          "Always Prepared"
          (DrawCard #after You (basic $ IsEncounterCard <> mapOneOf CardWithTrait traits) AnyDeck)
          Free
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
              chooseOneM iid do
                labeled "Attach to enemy (Nemesis)" do
                  place attrs $ AttachedToEnemy eid
                labeled "Do not attach to enemy" nothing
        TreacheryTarget tid -> do
          when (attrs `hasCustomization` MythosHardened) do
            treacheryTraits <- field TreacheryTraits tid
            when (any (`elem` traits) treacheryTraits) do
              chooseOneM iid do
                labeled "Add both the treachery and Grizzled to the victory display (Mythos-Hardened)" do
                  addToVictory attrs
                  addToVictory tid
                labeled "Do not add to victory" nothing
        _ -> pure ()
      pure s
    InDiscard _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      addToHand iid (only attrs)
      pure s
    _ -> Grizzled <$> liftRunMessage msg attrs
