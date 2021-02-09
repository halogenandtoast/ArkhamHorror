module Arkham.Types.Location.Cards.MuseumHalls
  ( museumHalls
  , MuseumHalls(..)
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MuseumHalls = MuseumHalls LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

museumHalls :: MuseumHalls
museumHalls = MuseumHalls $ base
  { locationConnectedSymbols = setFromList [Circle, Diamond, Triangle]
  }
 where
  base = baseAttrs
    "02127"
    (Name "Museum Halls" Nothing)
    EncounterSet.TheMiskatonicMuseum
    2
    (Static 0)
    Square
    [Circle]
    (singleton Miskatonic)

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
    withBaseActions iid NonFast location $ do
      clueCost <- getPlayerCountValue (PerPlayer 1)
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource location)
              1
              (ActionAbility Nothing $ Costs
                [ ActionCost 1
                , GroupClueCost
                  clueCost
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
      <$ unshiftMessage (UseScenarioSpecificAbility iid 1)
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
