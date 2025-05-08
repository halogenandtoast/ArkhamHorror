module Arkham.Helpers.Choices where

import Arkham.I18n
import Arkham.Id
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Source

chooseTakeHorrorAndDamage
  :: (ReverseQueue m, HasI18n, AsId investigator, IdOf investigator ~ InvestigatorId, Sourceable source)
  => investigator -> source -> Int -> Int -> ChooseT m ()
chooseTakeHorrorAndDamage iid attrs h d =
  unscoped
    $ numberVar "damage" d
    $ numberVar "horror" h
    $ labeled' "takeHorrorAndDamage"
    $ assignDamageAndHorror (asId iid) attrs d h

chooseTakeHorror
  :: (ReverseQueue m, HasI18n, AsId investigator, IdOf investigator ~ InvestigatorId, Sourceable source)
  => investigator -> source -> Int -> ChooseT m ()
chooseTakeHorror iid attrs h = unscoped $ countVar h $ labeled' "takeHorror" $ assignHorror (asId iid) attrs h

chooseLoseActions
  :: (ReverseQueue m, HasI18n, AsId investigator, IdOf investigator ~ InvestigatorId, Sourceable source)
  => investigator -> source -> Int -> ChooseT m ()
chooseLoseActions iid attrs n = unscoped $ countVar n $ labeled' "loseActions" $ loseActions (asId iid) attrs n
