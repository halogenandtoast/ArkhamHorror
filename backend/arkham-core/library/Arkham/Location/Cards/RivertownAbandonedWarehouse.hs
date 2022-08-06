module Arkham.Location.Cards.RivertownAbandonedWarehouse
  ( RivertownAbandonedWarehouse(..)
  , rivertownAbandonedWarehouse
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( rivertownAbandonedWarehouse )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait

newtype RivertownAbandonedWarehouse = RivertownAbandonedWarehouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertownAbandonedWarehouse :: LocationCard RivertownAbandonedWarehouse
rivertownAbandonedWarehouse = location
  RivertownAbandonedWarehouse
  Cards.rivertownAbandonedWarehouse
  4
  (PerPlayer 1)

instance HasAbilities RivertownAbandonedWarehouse where
  getAbilities (RivertownAbandonedWarehouse attrs) =
    withBaseAbilities attrs
      $ [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility attrs 1 Here
          $ ActionAbility Nothing
          $ Costs
              [ActionCost 1, HandDiscardCost 1 $ CardWithSkill SkillWillpower]
        | locationRevealed attrs
        ]

willpowerCount :: Payment -> Int
willpowerCount (DiscardCardPayment cards) =
  sum $ map (count (== SkillWillpower) . cdSkills . toCardDef) cards
willpowerCount (Payments xs) = sum $ map willpowerCount xs
willpowerCount _ = 0

instance RunMessage RivertownAbandonedWarehouse where
  runMessage msg l@(RivertownAbandonedWarehouse attrs) = case msg of
    UseCardAbility iid source _ 1 payments | isSource attrs source -> do
      let doomToRemove = willpowerCount payments
      cultists <- selectList $ EnemyWithTrait Cultist
      unless (null cultists) $ push $ chooseOne
        iid
        [ RemoveDoom (EnemyTarget eid) doomToRemove | eid <- cultists ]
      pure l
    _ -> RivertownAbandonedWarehouse <$> runMessage msg attrs
