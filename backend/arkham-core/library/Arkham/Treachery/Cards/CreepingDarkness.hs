module Arkham.Treachery.Cards.CreepingDarkness
  ( creepingDarkness
  , CreepingDarkness(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CreepingDarkness = CreepingDarkness TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creepingDarkness :: TreacheryCard CreepingDarkness
creepingDarkness = treachery CreepingDarkness Cards.creepingDarkness

instance HasModifiersFor CreepingDarkness where
  getModifiersFor (EnemyTarget eid) (CreepingDarkness a) = do
    isFormlessSpawn <- eid <=~> enemyIs Enemies.formlessSpawn
    n <- getPlayerCountValue (PerPlayer 1)
    pure $ toModifiers a [ HealthModifier n | isFormlessSpawn ]
  getModifiersFor _ _ = pure []

instance HasAbilities CreepingDarkness where
  getAbilities (CreepingDarkness a) =
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
        2
    ]

instance RunMessage CreepingDarkness where
  runMessage msg t@(CreepingDarkness attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      nexus <- selectJust $ locationIs Locations.nexusOfNKai
      pushAll
        [ AttachTreachery (toId attrs) $ LocationTarget nexus
        , PlaceDoom (toTarget attrs) 1
        ]
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasTorches <- getHasSupply iid Torches
      push
        $ chooseOrRunOne iid
        $ Label
            "Test {willpower} (3)"
            [ beginSkillTest iid (toSource attrs) (toTarget attrs) SkillWillpower 3 ]
        : [ Label "Check supplies" [Discard (toAbilitySource attrs 1) (toTarget attrs)] | hasTorches ]
      pure t
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
        pure t
    _ -> CreepingDarkness <$> runMessage msg attrs
