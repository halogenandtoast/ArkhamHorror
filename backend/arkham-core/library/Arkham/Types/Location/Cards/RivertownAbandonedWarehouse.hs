module Arkham.Types.Location.Cards.RivertownAbandonedWarehouse
  ( RivertownAbandonedWarehouse(..)
  , rivertownAbandonedWarehouse
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype RivertownAbandonedWarehouse = RivertownAbandonedWarehouse LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertownAbandonedWarehouse :: RivertownAbandonedWarehouse
rivertownAbandonedWarehouse = RivertownAbandonedWarehouse $ baseAttrs
  "50030"
  (Name "Rivertown" (Just "Abandoned Warehouse"))
  EncounterSet.ReturnToTheMidnightMasks
  4
  (PerPlayer 1)
  Circle
  [Moon, Diamond, Square, Squiggle, Hourglass]
  [Arkham, Central]

instance HasModifiersFor env RivertownAbandonedWarehouse where
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability attrs = base { abilityLimit = GroupLimit PerGame 1 }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ActionAbility Nothing $ Costs
      [ ActionCost 1
      , HandDiscardCost 1 Nothing mempty (singleton SkillWillpower)
      ]
    )

instance ActionRunner env => HasActions env RivertownAbandonedWarehouse where
  getActions iid NonFast (RivertownAbandonedWarehouse attrs)
    | locationRevealed attrs = withBaseActions iid NonFast attrs $ do
      pure [ ActivateCardAbilityAction iid (ability attrs) | iid `on` attrs ]
  getActions iid window (RivertownAbandonedWarehouse attrs) =
    getActions iid window attrs

willpowerCount :: Payment -> Int
willpowerCount (DiscardPayment cards) =
  sum $ map (count (== SkillWillpower) . pcSkills) cards
willpowerCount (Payments xs) = sum $ map willpowerCount xs
willpowerCount _ = 0

instance LocationRunner env => RunMessage env RivertownAbandonedWarehouse where
  runMessage msg l@(RivertownAbandonedWarehouse attrs) = case msg of
    UseCardAbility iid source Nothing 1 payments | isSource attrs source -> do
      let doomToRemove = willpowerCount payments
      cultists <- getSetList Cultist
      l <$ unless
        (null cultists)
        (unshiftMessage
          (chooseOne
            iid
            [ RemoveDoom (EnemyTarget eid) doomToRemove | eid <- cultists ]
          )
        )
    _ -> RivertownAbandonedWarehouse <$> runMessage msg attrs
