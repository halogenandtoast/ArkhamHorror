module Arkham.Types.Location.Cards.ATearInThePath
  ( aTearInThePath
  , ATearInThePath(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (aTearInThePath)
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

newtype ATearInThePath = ATearInThePath LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInThePath :: LocationId -> ATearInThePath
aTearInThePath =
  ATearInThePath
    . (revealedSymbolL .~ Equals)
    . (revealedConnectedSymbolsL .~ setFromList [Square, Squiggle])
    . (unrevealedNameL .~ "Altered Path")
    . baseAttrs
        Cards.aTearInThePath
        3
        (PerPlayer 1)
        NoSymbol
        []

instance HasModifiersFor env ATearInThePath where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasActions env ATearInThePath where
  getActions iid (AfterRevealLocation You) (ATearInThePath attrs)
    | iid `on` attrs = do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      pure
        [ ActivateCardAbilityAction iid (forcedAbility attrs)
        | actionRemainingCount == 0
        ]
  getActions iid window (ATearInThePath attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env ATearInThePath where
  runMessage msg l@(ATearInThePath attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> ATearInThePath <$> runMessage msg attrs
