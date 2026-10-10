module Arkham.Homebrew.AgesUnwound.Enemies.PanzerIV (panzerIV) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Enemy.Import.Lifted hiding (PhaseStep)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype PanzerIV = PanzerIV EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /German War Machine/. Massive is on the card def.
panzerIV :: EnemyCard PanzerIV
panzerIV = enemy PanzerIV Cards.panzerIV

-- | "each investigator in each connecting location"
atConnected :: EnemyId -> InvestigatorMatcher
atConnected eid = InvestigatorAt $ connectedFrom $ locationWithEnemy eid

{- | "Forced - When enemies attack during the enemy phase, if Panzer IV is ready
and unengaged: It attacks each investigator in each connecting location. If no
attacks are made this way, move Panzer IV once towards the nearest
investigator."

Mirrors /Eztli Guardian/, which prints the same reach-into-the-next-room attack,
minus the move. Note that Massive engages every investigator at Panzer IV's own
location, so "unengaged" is in practice "nobody is standing on it".
-}
instance HasAbilities PanzerIV where
  getAbilities (PanzerIV a) =
    extend1 a
      $ groupLimit PerPhase
      $ restricted a 1 (exists $ AnyEnemy <> be a <> #ready <> #unengaged)
      $ forced
      $ PhaseStep #when EnemiesAttackStep

instance RunMessage PanzerIV where
  runMessage msg e@(PanzerIV attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      targets <- select $ atConnected attrs.id
      if null targets
        then push $ MoveToward (toTarget attrs) (LocationWithInvestigator Anyone)
        else for_ targets \iid ->
          push
            $ EnemyWillAttack
            $ (enemyAttack attrs.id (attrs.ability 1) iid)
              {attackDamageStrategy = enemyDamageStrategy attrs}
      pure e
    _ -> PanzerIV <$> liftRunMessage msg attrs
