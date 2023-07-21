module Arkham.Treachery.Cards.SearchingForIzzie (
  SearchingForIzzie (..),
  searchingForIzzie,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message hiding (InvestigatorEliminated)
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SearchingForIzzie = SearchingForIzzie TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForIzzie :: TreacheryCard SearchingForIzzie
searchingForIzzie = treachery SearchingForIzzie Cards.searchingForIzzie

instance HasAbilities SearchingForIzzie where
  getAbilities (SearchingForIzzie x) =
    restrictedAbility
      x
      1
      OnSameLocation
      (ActionAbility (Just Action.Investigate) $ ActionCost 2)
      : [ mkAbility x 2 $
          ForcedAbility $
            OrWindowMatcher
              [ GameEnds Timing.When
              , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
              ]
        | iid <- maybeToList (treacheryOwner x)
        ]

instance RunMessage SearchingForIzzie where
  runMessage msg t@(SearchingForIzzie attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      targets <- selectListMap LocationTarget $ FarthestLocationFromYou Anywhere
      case targets of
        [] -> pure ()
        xs ->
          push $
            chooseOrRunOne
              iid
              [ TargetLabel target [AttachTreachery treacheryId target]
              | target <- xs
              ]
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      withTreacheryLocation attrs $ \locationId -> do
        skillType <- field LocationInvestigateSkill locationId
        push $
          Investigate
            iid
            locationId
            source
            (Just $ toTarget attrs)
            skillType
            False
        pure t
    Successful (Action.Investigate, _) _ _ target _
      | isTarget attrs target ->
          t <$ push (Discard (toAbilitySource attrs 1) target)
    UseCardAbility _ source 2 _ _
      | isSource attrs source ->
          let investigator = fromJustNote "missing investigator" treacheryOwner
          in  t <$ push (SufferTrauma investigator 0 1)
    _ -> SearchingForIzzie <$> runMessage msg attrs
