module Arkham.Enemy.Cards.PrimevalTerror (primevalTerror) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Keyword (Keyword (Patrol))
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Trait (Trait (Summit))

newtype PrimevalTerror = PrimevalTerror EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primevalTerror :: EnemyCard PrimevalTerror
primevalTerror = enemy PrimevalTerror Cards.primevalTerror

instance HasModifiersFor PrimevalTerror where
  getModifiersFor (PrimevalTerror a) = do
    modifySelf
      a
      [ CannotMakeAttacksOfOpportunity
      , -- "Patrol (nearest empty Summit location)" — empty of investigators and
        -- enemies both, which is what @EmptyLocation@ means.
        AddKeyword (Patrol $ NearestLocationToYou $ LocationWithTrait Summit <> EmptyLocation)
      ]
    -- "While engaged with this enemy, you may enter open sky as if it were a
    -- location." Open sky already is a location; what stops entry is the
    -- CannotEnter the Open Sky card applies, so engaged investigators are exempted.
    modifySelect a (InvestigatorEngagedWith (be a)) [canEnterOpenSky]

instance HasAbilities PrimevalTerror where
  getAbilities (PrimevalTerror a) =
    extend1 a
      $ mkAbility a 1
      $ SilentForcedAbility
      $ oneOf
        [ EnemyDefeated #after (InvestigatorAt isOpenSky) ByAny (be a)
        , EnemyDisengaged #after (InvestigatorAt isOpenSky) (be a)
        ]

instance RunMessage PrimevalTerror where
  runMessage msg e@(PrimevalTerror attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "If you defeat or disengage from this enemy while at open sky, you are
      -- defeated and suffer 1 physical trauma." Nothing is holding you up there
      -- any more.
      sufferPhysicalTrauma iid 1
      investigatorDefeated (attrs.ability 1) iid
      pure e
    _ -> PrimevalTerror <$> liftRunMessage msg attrs
