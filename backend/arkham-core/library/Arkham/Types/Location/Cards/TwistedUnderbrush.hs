module Arkham.Types.Location.Cards.TwistedUnderbrush
  ( TwistedUnderbrush(..)
  , twistedUnderbrush
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype TwistedUnderbrush = TwistedUnderbrush LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedUnderbrush :: TwistedUnderbrush
twistedUnderbrush = TwistedUnderbrush $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "81015"
    (Name "Twisted Underbrush" Nothing)
    EncounterSet.CurseOfTheRougarou
    3
    (PerPlayer 1)
    Moon
    [Diamond, Moon]
    [Wilderness]

instance HasModifiersFor env TwistedUnderbrush where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env TwistedUnderbrush where
  getActions iid NonFast (TwistedUnderbrush attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
      | iid `member` locationInvestigators
      ]
  getActions i window (TwistedUnderbrush attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env TwistedUnderbrush where
  runMessage msg l@(TwistedUnderbrush attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessages
        [ TakeResources iid 2 False
        , InvestigatorAssignDamage iid source DamageAny 0 1
        ]
    _ -> TwistedUnderbrush <$> runMessage msg attrs
