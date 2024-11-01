module Arkham.Investigator.Cards.WendyAdamsParallel (wendyAdamsParallel, WendyAdamsParallel (..)) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, getSkillTestTarget, withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (EnemyEvaded)
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype WendyAdamsParallel = WendyAdamsParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

wendyAdamsParallel :: InvestigatorCard WendyAdamsParallel
wendyAdamsParallel =
  investigator WendyAdamsParallel Cards.wendyAdamsParallel
    $ Stats {health = 7, sanity = 7, willpower = 4, intellect = 3, combat = 1, agility = 4}

instance HasAbilities WendyAdamsParallel where
  getAbilities (WendyAdamsParallel a) =
    [ playerLimit PerTestOrAbility
        $ restricted a 1 Self
        $ freeReaction
        $ SkillTestResult #after You #evading #success
    ]

instance HasChaosTokenValue WendyAdamsParallel where
  getChaosTokenValue iid ElderSign (WendyAdamsParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage WendyAdamsParallel where
  runMessage msg i@(WendyAdamsParallel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      revealedTokens <-
        filter ((`elem` [CurseToken, BlessToken]) . (.face)) <$> getSkillTestRevealedChaosTokens
      inBag <- select $ oneOf [ChaosTokenFaceIs #bless, ChaosTokenFaceIs #curse]

      chooseOneM iid do
        when (notNull inBag) do
          labeled "Seal 1 {bless} or {curse} token from the chaos bag" $ doStep 1 msg
        when (notNull revealedTokens) do
          labeled "Seal any {bless} or {curse} tokens revealed from the chaos bag during this test" do
            doStep 2 msg
      pure i
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> do
          inBag <- select $ oneOf [ChaosTokenFaceIs #bless, ChaosTokenFaceIs #curse]
          push $ FocusChaosTokens inBag
          chooseOneM iid do
            targets inBag \token -> do
              push $ SealChaosToken token
              push $ SealedChaosToken token (toTarget eid)
          push $ UnfocusChaosTokens
        _ -> pure ()
      pure i
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> do
          revealedTokens <-
            filter ((`elem` [CurseToken, BlessToken]) . (.face)) <$> getSkillTestRevealedChaosTokens
          push $ FocusChaosTokens revealedTokens
          chooseUpToNM iid (length revealedTokens) "Done sealing tokens" do
            targets revealedTokens \token -> do
              push $ SealChaosToken token
              push $ SealedChaosToken token (toTarget eid)
          push $ UnfocusChaosTokens
        _ -> pure ()
      pure i
    ElderSignEffect (is attrs -> True) -> do
      withSkillTest \sid -> do
        tokens <- select $ oneOf [ChaosTokenFaceIs #bless, ChaosTokenFaceIs #curse]
        when (notNull tokens) do
          push $ FocusChaosTokens tokens
          chooseUpToNM attrs.id 2 "Do not choose any more tokens" do
            targets tokens \token -> do
              skillTestModifiers
                sid
                attrs
                token
                [IgnoreChaosTokenModifier, IgnoreChaosTokenEffects, ReturnCursedToChaosBag, ReturnBlessedToChaosBag]
              push $ RevealChaosToken (SkillTestSource sid) attrs.id token
              push $ RevealSkillTestChaosTokensAgain attrs.id
          push UnfocusChaosTokens

      pure i
    _ -> WendyAdamsParallel <$> liftRunMessage msg attrs
