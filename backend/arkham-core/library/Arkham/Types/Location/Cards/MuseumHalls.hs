module Arkham.Types.Location.Cards.MuseumHalls
  ( museumHalls
  , MuseumHalls(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (museumHalls)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype MuseumHalls = MuseumHalls LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

museumHalls :: LocationCard MuseumHalls
museumHalls = locationWithRevealedSideConnectionsWith
  MuseumHalls
  Cards.museumHalls
  2
  (Static 0)
  Square
  [Circle]
  Square
  [Circle, Diamond, Triangle]
  (revealedConnectedMatchersL <>~ [LocationWithTitle "Exhibit Hall"])

instance HasModifiersFor env MuseumHalls where
  getModifiersFor _ target (MuseumHalls l) | isTarget l target =
    pure $ toModifiers l [ Blocked | unrevealed l ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MuseumHalls where
  getAbilities (MuseumHalls attrs) | unrevealed attrs = withBaseAbilities
    attrs
    [ restrictedAbility
        (ProxySource
          (LocationMatcherSource $ LocationWithTitle "Museum Entrance")
          (toSource attrs)
        )
        1
        (OnLocation $ LocationWithTitle "Museum Entrance")
        (ActionAbility Nothing $ ActionCost 1)
    ]
  getAbilities (MuseumHalls attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ Costs
        [ ActionCost 1
        , GroupClueCost (PerPlayer 1) (LocationWithTitle "Museum Halls")
        ]
    ]

instance LocationRunner env => RunMessage env MuseumHalls where
  runMessage msg l@(MuseumHalls attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) _ 1 _
      | isSource attrs source && unrevealed attrs -> do
        museumEntrance <- fromJustNote "missing location"
          <$> selectOne (LocationWithTitle "Museum Entrance")
        l <$ push
          (BeginSkillTest
            iid
            source
            (LocationTarget museumEntrance)
            Nothing
            SkillCombat
            5
          )
    UseCardAbility iid source _ 1 _ | isSource attrs source && revealed attrs ->
      l <$ push (UseScenarioSpecificAbility iid Nothing 1)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        actId <- fromJustNote "missing act" . headMay <$> getSetList ()
        l <$ push (AdvanceAct actId source)
    _ -> MuseumHalls <$> runMessage msg attrs
