module Arkham.Homebrew.AgesUnwound.Treacheries.UnleashedChaosMutation (unleashedChaosMutation) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype UnleashedChaosMutation = UnleashedChaosMutation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unleashedChaosMutation :: TreacheryCard UnleashedChaosMutation
unleashedChaosMutation = treachery UnleashedChaosMutation Cards.unleashedChaosIMutationI

{- | "while resolving an ability on the attached card, or while attacking or
evading the attached enemy"

@Source.enemy@/@Source.treachery@ both unwrap an 'AbilitySource', so matching the
attached entity as the test's source covers its abilities too.
-}
attachedTests :: TreacheryAttrs -> Maybe SkillTestMatcher
attachedTests a = case a.attached of
  Just (EnemyTarget eid) ->
    Just
      $ SkillTestOneOf
        [ SkillTestSourceMatches (SourceIsEnemy $ EnemyWithId eid)
        , WhileAttackingAnEnemy (EnemyWithId eid)
        , WhileEvadingAnEnemy (EnemyWithId eid)
        ]
  Just (TreacheryTarget tid) ->
    Just $ SkillTestSourceMatches (SourceIsTreacheryEffect $ TreacheryWithId tid)
  _ -> Nothing

{- | "Forced - When you would succeed at a skill test while resolving an ability
on the attached card, or while attacking or evading the attached enemy: Reveal
and resolve an additional chaos token. (Limit once per test.)"
-}
instance HasAbilities UnleashedChaosMutation where
  getAbilities (UnleashedChaosMutation a) = case attachedTests a of
    Nothing -> []
    Just stm ->
      [playerLimit PerTest $ mkAbility a 1 $ forced $ WouldHaveSkillTestResult #when You stm #success]

{- | "Revelation - Draw the top card of the encounter deck and attach Unleashed
Chaos (Mutation) to it."
-}
instance RunMessage UnleashedChaosMutation where
  runMessage msg t@(UnleashedChaosMutation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mcard <- headMay <$> getEncounterDeck
      case mcard of
        Nothing -> toDiscard attrs attrs
        Just card -> do
          drawEncounterCard iid attrs
          push $ HandleTargetChoice iid (toSource attrs) (CardIdTarget $ toCardId card)
      pure t
    HandleTargetChoice _ (isSource attrs -> True) (CardIdTarget cid) -> do
      menemy <- selectOne (EnemyWithCardId cid)
      mtreachery <- selectOne (TreacheryWithCardId cid)
      case (menemy, mtreachery) of
        (Just eid, _) -> attachTreachery attrs eid
        (_, Just tid) -> placeTreachery attrs (AttachedToTreachery tid)
        -- TODO(ages-unwound): a drawn card that leaves nothing in play (most
        -- treacheries) has nothing to attach to, so Mutation is discarded
        _ -> toDiscard attrs attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DrawAnotherChaosToken iid
      pure t
    _ -> UnleashedChaosMutation <$> liftRunMessage msg attrs
