module Arkham.Types.Location.Cards.SlaughteredWoods
  ( slaugteredWoods
  , SlaughteredWoods(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (slaugteredWoods)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Window

newtype SlaughteredWoods = SlaughteredWoods LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slaugteredWoods :: LocationId -> SlaughteredWoods
slaugteredWoods =
  SlaughteredWoods
    . (revealedSymbolL .~ Plus)
    . (revealedConnectedSymbolsL .~ setFromList [Triangle, Hourglass])
    . (unrevealedNameL .~ "Diverging Path")
    . baseAttrs
        Cards.slaugteredWoods
        2
        (PerPlayer 1)
        NoSymbol
        []

instance HasModifiersFor env SlaughteredWoods where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env SlaughteredWoods where
  getActions iid (AfterRevealLocation You) (SlaughteredWoods attrs)
    | iid `on` attrs = do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      pure
        [ ActivateCardAbilityAction iid (forcedAbility attrs)
        | actionRemainingCount == 0
        ]
  getActions iid window (SlaughteredWoods attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env SlaughteredWoods where
  runMessage msg l@(SlaughteredWoods attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny 0 2)
    _ -> SlaughteredWoods <$> runMessage msg attrs
