module Arkham.Types.Location.Cards.LostMemories
  ( lostMemories
  , LostMemories(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Trait
import Arkham.Types.Window

newtype LostMemories = LostMemories LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: LostMemories
lostMemories =
  LostMemories
    $ base
    & (revealedSymbolL .~ T)
    & (revealedConnectedSymbolsL .~ setFromList [Square, Moon])
 where
  base = baseAttrs
    "02292"
    (Name "Lost Memories" Nothing)
    EncounterSet.WhereDoomAwaits
    2
    (PerPlayer 1)
    NoSymbol
    []
    [Dunwich, Woods, Altered]

instance HasModifiersFor env LostMemories where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env LostMemories where
  getActions iid (AfterRevealLocation You) (LostMemories attrs)
    | iid `on` attrs = pure
      [ActivateCardAbilityAction iid (forcedAbility attrs)]
  getActions iid window (LostMemories attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env LostMemories where
  runMessage msg l@(LostMemories attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      l <$ unshiftMessages
        [ InvestigatorAssignDamage iid source DamageAny 0 actionRemainingCount
        | actionRemainingCount > 0
        ]
    _ -> LostMemories <$> runMessage msg attrs
