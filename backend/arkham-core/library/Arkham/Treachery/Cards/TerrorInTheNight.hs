module Arkham.Treachery.Cards.TerrorInTheNight (
  terrorInTheNight,
  TerrorInTheNight (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TerrorInTheNight = TerrorInTheNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorInTheNight :: TreacheryCard TerrorInTheNight
terrorInTheNight = treachery TerrorInTheNight Cards.terrorInTheNight

instance RunMessage TerrorInTheNight where
  runMessage msg t@(TerrorInTheNight attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ RevelationSkillTest iid (toSource attrs) SkillWillpower 4
      pure t
    FailedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n ->
      do
        aid <- selectJust AnyAgenda
        other <- selectList $ treacheryIs Cards.terrorInTheNight
        iids <- getInvestigatorIds
        attached <-
          filterM
            (fieldMap TreacheryPlacement (== TreacheryAttachedTo (toTarget aid)))
            other
        pushAll
          $ AttachTreachery (toId attrs) (toTarget aid)
          : [gainSurge attrs | n >= 3]
            <> ( guard (length attached >= 2)
                  *> ( toDiscard (toSource attrs) (toTarget attrs)
                        : map (toDiscard (toSource attrs) . toTarget) attached
                          <> [assignHorror iid attrs 3 | iid <- iids]
                     )
               )
        pure t
    _ -> TerrorInTheNight <$> runMessage msg attrs
