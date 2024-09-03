module Arkham.Investigator.Cards.WendyAdamsParallel (
  wendyAdamsParallel,
  WendyAdamsParallel (..),
)
where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, getSkillTestTarget, withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (EnemyEvaded)
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Modifier
import Arkham.Projection

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
        $ restrictedAbility a 1 Self
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

      chooseOne iid
        $ [Label "Seal 1 {bless} or {curse} token from the chaos bag" [DoStep 1 msg] | notNull inBag]
        <> [ Label
            "Seal any {bless} or {curse} tokens revealed from the chaos bag during this test"
            [DoStep 2 msg]
           | notNull revealedTokens
           ]

      pure i
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> do
          inBag <- select $ oneOf [ChaosTokenFaceIs #bless, ChaosTokenFaceIs #curse]
          card <- field EnemyCard eid
          push $ FocusChaosTokens inBag
          chooseOne
            iid
            [ targetLabel token [SealChaosToken token, SealedChaosToken token card]
            | token <- inBag
            ]
          push $ UnfocusChaosTokens
        _ -> pure ()
      pure i
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> do
          revealedTokens <-
            filter ((`elem` [CurseToken, BlessToken]) . (.face)) <$> getSkillTestRevealedChaosTokens
          card <- field EnemyCard eid
          push $ FocusChaosTokens revealedTokens
          chooseUpToN
            iid
            (length revealedTokens)
            "Done sealing tokens"
            [ targetLabel token [SealChaosToken token, SealedChaosToken token card]
            | token <- revealedTokens
            ]
          push $ UnfocusChaosTokens
        _ -> pure ()
      pure i
    ElderSignEffect (is attrs -> True) -> do
      withSkillTest \sid -> do
        tokens <- select $ oneOf [ChaosTokenFaceIs #bless, ChaosTokenFaceIs #curse]
        when (notNull tokens) do
          push $ FocusChaosTokens tokens
          chooseUpToN
            attrs.id
            2
            "Do not choose any more tokens"
            [ targetLabel
              token
              [ Msg.skillTestModifiers
                  sid
                  attrs
                  token
                  [IgnoreChaosTokenModifier, IgnoreChaosTokenEffects, ReturnCursedToChaosBag, ReturnBlessedToChaosBag]
              , RevealChaosToken (SkillTestSource sid) attrs.id token
              , RevealSkillTestChaosTokensAgain attrs.id
              ]
            | token <- tokens
            ]
          push UnfocusChaosTokens

      pure i
    _ -> WendyAdamsParallel <$> liftRunMessage msg attrs
