module Arkham.Types.Treachery.Cards.SearchingForIzzie
  ( SearchingForIzzie(..)
  , searchingForIzzie
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (InvestigatorEliminated)
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SearchingForIzzie = SearchingForIzzie TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForIzzie :: TreacheryCard SearchingForIzzie
searchingForIzzie = treachery SearchingForIzzie Cards.searchingForIzzie

instance HasAbilities env SearchingForIzzie where
  getAbilities _ _ (SearchingForIzzie x) =
    pure
      $ restrictedAbility
          x
          1
          OnSameLocation
          (ActionAbility (Just Action.Investigate) $ ActionCost 2)
      : [ mkAbility x 1 $ ForcedAbility $ OrWindowMatcher
            [ GameEnds Timing.When
            , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
            ]
        | iid <- maybeToList (treacheryOwner x)
        ]

instance TreacheryRunner env => RunMessage env SearchingForIzzie where
  runMessage msg t@(SearchingForIzzie attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      targets <- selectListMap LocationTarget $ FarthestLocationFromYou Anywhere
      t <$ case targets of
        [] -> push (Discard $ toTarget attrs)
        xs ->
          push
            (chooseOrRunOne
              iid
              [ AttachTreachery treacheryId target | target <- xs ]
            )
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      withTreacheryLocation attrs $ \attachedLocationId -> do
        shroud <- unShroud <$> getCount attachedLocationId
        t <$ push
          (BeginSkillTest
            iid
            source
            (InvestigatorTarget iid)
            (Just Action.Investigate)
            SkillIntellect
            shroud
          )
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ push (Discard $ toTarget attrs)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ push (SufferTrauma investigator 0 1)
    _ -> SearchingForIzzie <$> runMessage msg attrs
