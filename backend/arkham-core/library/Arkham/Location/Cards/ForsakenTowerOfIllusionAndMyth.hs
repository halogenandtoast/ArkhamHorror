module Arkham.Location.Cards.ForsakenTowerOfIllusionAndMyth (
  forsakenTowerOfIllusionAndMyth,
  ForsakenTowerOfIllusionAndMyth (..),
)
where

import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.SkillTest (investigate)
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.WhereTheGodsDwell.Helpers

newtype ForsakenTowerOfIllusionAndMyth = ForsakenTowerOfIllusionAndMyth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfIllusionAndMyth :: LocationCard ForsakenTowerOfIllusionAndMyth
forsakenTowerOfIllusionAndMyth =
  locationWith
    ForsakenTowerOfIllusionAndMyth
    Cards.forsakenTowerOfIllusionAndMyth
    5
    (PerPlayer 1)
    (setMeta @(Maybe EnemyId) Nothing)

instance HasAbilities ForsakenTowerOfIllusionAndMyth where
  getAbilities (ForsakenTowerOfIllusionAndMyth attrs) = extendRevealed attrs $ forsakenTowerAbilities attrs

instance RunMessage ForsakenTowerOfIllusionAndMyth where
  runMessage msg l@(ForsakenTowerOfIllusionAndMyth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      revealWhisperingChaos attrs
      nyarlathoteps <- select $ EnemyInHandOf $ InvestigatorWithId iid
      chooseOne
        iid
        [ targetLabel
          nyarlathotep
          [HandleTargetChoice iid (attrs.ability 1) (toTarget nyarlathotep)]
        | nyarlathotep <- nyarlathoteps
        ]
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget nyarlathotep) -> do
      push
        $ investigate
          iid
          (attrs.ability 1)
          (toTarget attrs)
          #intellect
          (EnemyMaybeGameValueFieldCalculation nyarlathotep EnemyHealthActual)
      pure $ ForsakenTowerOfIllusionAndMyth $ setMeta (Just nyarlathotep) attrs
    Successful (Action.Investigate, other) iid (isAbilitySource attrs 1 -> True) target n -> do
      discardWhisperingChaos attrs
      attrs' <-
        lift (runMessage (Successful (Action.Investigate, other) iid (toSource attrs) target n) attrs)
      case toResult @(Maybe EnemyId) attrs.meta of
        Nothing -> error "Invalid meta"
        Just nyarlathotep -> do
          push $ AddToVictory (toTarget nyarlathotep)
      pure $ ForsakenTowerOfIllusionAndMyth attrs'
    Failed (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      shuffleWhisperingChaosBackIntoEncounterDeck attrs
      case toResult @(Maybe EnemyId) attrs.meta of
        Nothing -> error "Invalid meta"
        Just nyarlathotep -> do
          pushAll
            [ InitiateEnemyAttack $ enemyAttack nyarlathotep (attrs.ability 1) iid
            , ShuffleBackIntoEncounterDeck (toTarget nyarlathotep)
            ]
      pure l
    _ -> ForsakenTowerOfIllusionAndMyth <$> liftRunMessage msg attrs
