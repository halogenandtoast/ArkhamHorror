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
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.Window hiding (EndOfGame)

newtype SearchingForIzzie = SearchingForIzzie TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForIzzie :: TreacheryCard SearchingForIzzie
searchingForIzzie = treachery SearchingForIzzie Cards.searchingForIzzie

instance HasModifiersFor env SearchingForIzzie

instance ActionRunner env => HasAbilities env SearchingForIzzie where
  getAbilities iid (Window Timing.When NonFast) (SearchingForIzzie attrs) = do
    investigatorLocationId <- getId @LocationId iid
    pure
      [ mkAbility attrs 1 $ ActionAbility Nothing $ ActionCost 2
      | treacheryOnLocation investigatorLocationId attrs
      ]
  getAbilities _ _ _ = pure []

instance TreacheryRunner env => RunMessage env SearchingForIzzie where
  runMessage msg t@(SearchingForIzzie attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      farthestLocations <- map unFarthestLocationId <$> getSetList iid
      t <$ case farthestLocations of
        [lid] -> push (AttachTreachery treacheryId (LocationTarget lid))
        lids -> push
          (chooseOne
            iid
            [ AttachTreachery treacheryId (LocationTarget lid) | lid <- lids ]
          )
    UseCardAbility iid (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      withTreacheryLocation attrs $ \attachedLocationId -> do
        shroud <- unShroud <$> getCount attachedLocationId
        t <$ push
          (BeginSkillTest
            iid
            (TreacherySource treacheryId)
            (InvestigatorTarget iid)
            (Just Action.Investigate)
            SkillIntellect
            shroud
          )
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ push (Discard $ toTarget attrs)
    EndOfGame ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ push (SufferTrauma investigator 0 1)
    _ -> SearchingForIzzie <$> runMessage msg attrs
