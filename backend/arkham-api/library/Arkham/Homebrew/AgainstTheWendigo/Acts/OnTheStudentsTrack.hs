module Arkham.Homebrew.AgainstTheWendigo.Acts.OnTheStudentsTrack (onTheStudentsTrack) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (drawStudentsFate)
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype OnTheStudentsTrack = OnTheStudentsTrack ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheStudentsTrack :: ActCard OnTheStudentsTrack
onTheStudentsTrack = act (2, A) OnTheStudentsTrack Cards.onTheStudentsTrack Nothing

instance HasAbilities OnTheStudentsTrack where
  getAbilities = actAbilities \x ->
    [ restricted x 1 (exists $ InvestigatorAt Anywhere)
        $ FastAbility
        $ GroupClueCost (PerPlayer 2) Anywhere
    , onlyOnce
        $ restricted x 2 (foldMap hasRecordCriteria allFates)
        $ Objective
        $ forced AnyWindow
    ]
   where
    allFates =
      [ YouHaveDiscoveredBernardsFate
      , YouHaveDiscoveredNormansFate
      , YouHaveDiscoveredSylviasFate
      ]

instance RunMessage OnTheStudentsTrack where
  runMessage msg a@(OnTheStudentsTrack attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      drawStudentsFate
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record YouHaveDiscoveredTheFateOfDrNadelmannsStudents
      advanceActDeck attrs
      pure a
    _ -> OnTheStudentsTrack <$> liftRunMessage msg attrs
