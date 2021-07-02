module Arkham.Types.Location.Cards.MuseumHalls
  ( museumHalls
  , MuseumHalls(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (museumHalls)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype MuseumHalls = MuseumHalls LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

museumHalls :: LocationId -> MuseumHalls
museumHalls =
  MuseumHalls
    . (connectedSymbolsL .~ setFromList [Circle, Diamond, Triangle])
    . baseAttrs
        Cards.museumHalls
        2
        (Static 0)
        Square
        [Circle]

instance HasModifiersFor env MuseumHalls where
  getModifiersFor _ target (MuseumHalls location) | isTarget location target =
    pure $ toModifiers location [ Blocked | unrevealed location ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env MuseumHalls where
  getActions iid NonFast (MuseumHalls location) | unrevealed location =
    withBaseActions iid NonFast location $ do
      lid <- fromJustNote "missing location"
        <$> getLocationIdWithTitle "Museum Entrance"
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (ProxySource (LocationSource lid) (toSource location))
              1
              (ActionAbility Nothing $ ActionCost 1)
            )
        ]
  getActions iid NonFast (MuseumHalls location) | revealed location =
    withBaseActions iid NonFast location $ pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource location)
            1
            (ActionAbility Nothing $ Costs
              [ ActionCost 1
              , GroupClueCost
                (PerPlayer 1)
                (Just $ LocationWithTitle "Museum Halls")
              ]
            )
          )
      ]
  getActions iid window (MuseumHalls attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env MuseumHalls where
  runMessage msg l@(MuseumHalls location) = case msg of
    UseCardAbility iid (ProxySource _ source) _ 1 _
      | isSource location source && unrevealed location -> do
        museumEntrance <- fromJustNote "missing location"
          <$> getLocationIdWithTitle "Museum Entrance"
        l <$ unshiftMessage
          (BeginSkillTest
            iid
            source
            (LocationTarget museumEntrance)
            Nothing
            SkillCombat
            5
          )
    UseCardAbility iid source _ 1 _
      | isSource location source && revealed location -> l
      <$ unshiftMessage (UseScenarioSpecificAbility iid Nothing 1)
    PassedSkillTest _ _ source _ _ _ | isSource location source -> do
      actId <- fromJustNote "missing act" . headMay <$> getSetList ()
      l <$ unshiftMessage (AdvanceAct actId source)
    AddConnection lid _ | locationId location /= lid -> do
      name <- nameTitle <$> getName lid
      if name == "Exhibit Hall"
        then MuseumHalls
          <$> runMessage msg (location & connectedLocationsL %~ insertSet lid)
        else MuseumHalls <$> runMessage msg location
    _ -> MuseumHalls <$> runMessage msg location
