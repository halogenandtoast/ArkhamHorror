module Arkham.Types.Location.Cards.LostMemories
  ( lostMemories
  , LostMemories(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (lostMemories)
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

newtype LostMemories = LostMemories LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: LocationId -> LostMemories
lostMemories =
  LostMemories
    . (revealedSymbolL .~ T)
    . (revealedConnectedSymbolsL .~ setFromList [Square, Moon])
    . (unrevealedNameL .~ "Altered Path")
    . baseAttrs
        Cards.lostMemories
        2
        (PerPlayer 1)
        NoSymbol
        []

instance HasModifiersFor env LostMemories where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env LostMemories where
  getActions iid (AfterRevealLocation You) (LostMemories attrs)
    | iid `on` attrs = do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      pure
        [ ActivateCardAbilityAction iid (forcedAbility attrs)
        | actionRemainingCount > 0
        ]
  getActions iid window (LostMemories attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env LostMemories where
  runMessage msg l@(LostMemories attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      l <$ unshiftMessage
        (InvestigatorAssignDamage iid source DamageAny 0 actionRemainingCount)
    _ -> LostMemories <$> runMessage msg attrs
