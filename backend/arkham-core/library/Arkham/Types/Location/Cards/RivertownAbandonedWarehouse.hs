module Arkham.Types.Location.Cards.RivertownAbandonedWarehouse
  ( RivertownAbandonedWarehouse(..)
  , rivertownAbandonedWarehouse
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (rivertownAbandonedWarehouse)
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait

newtype RivertownAbandonedWarehouse = RivertownAbandonedWarehouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertownAbandonedWarehouse :: LocationCard RivertownAbandonedWarehouse
rivertownAbandonedWarehouse = location
  RivertownAbandonedWarehouse
  Cards.rivertownAbandonedWarehouse
  4
  (PerPlayer 1)
  Circle
  [Moon, Diamond, Square, Squiggle, Hourglass]

instance HasAbilities RivertownAbandonedWarehouse where
  getAbilities (RivertownAbandonedWarehouse attrs) =
    withBaseAbilities attrs $
      [ mkAbility
            attrs
            1
            (ActionAbility Nothing $ Costs
              [ ActionCost 1
              , HandDiscardCost 1 Nothing mempty (singleton SkillWillpower)
              ]
            )
          & (abilityLimitL .~ GroupLimit PerGame 1)
      | locationRevealed attrs
      ]

willpowerCount :: Payment -> Int
willpowerCount (DiscardCardPayment cards) =
  sum $ map (count (== SkillWillpower) . cdSkills . toCardDef) cards
willpowerCount (Payments xs) = sum $ map willpowerCount xs
willpowerCount _ = 0

instance LocationRunner env => RunMessage env RivertownAbandonedWarehouse where
  runMessage msg l@(RivertownAbandonedWarehouse attrs) = case msg of
    UseCardAbility iid source _ 1 payments | isSource attrs source -> do
      let doomToRemove = willpowerCount payments
      cultists <- getSetList Cultist
      l <$ unless
        (null cultists)
        (push
          (chooseOne
            iid
            [ RemoveDoom (EnemyTarget eid) doomToRemove | eid <- cultists ]
          )
        )
    _ -> RivertownAbandonedWarehouse <$> runMessage msg attrs
