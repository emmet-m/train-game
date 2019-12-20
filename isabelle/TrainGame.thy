theory TrainGame 
  imports
    Main
    "HOL-Library.Multiset"
begin

definition shuffles :: "'a list \<Rightarrow> ('a list) list" where
  "shuffles xs = map (\<lambda> n. drop n xs @ take n xs ) [0..<(length xs)]"

lemma shuffles_length[simp]: "length (shuffles xs) = length xs"
  apply (induct xs)
   apply (simp add: shuffles_def)+
  done

lemma shuffles_ith[simp]: "i < length xs \<Longrightarrow> shuffles xs ! i = drop i xs @ take i xs"
  by (simp add: shuffles_def)

lemma shuffles_lists_length[simp]: "i < length xs \<Longrightarrow> length (shuffles xs ! i) = length xs"
  apply (induct xs arbitrary: i)
   apply simp
  apply auto 
  done

lemma shuffles_lists_elems1[simp]: "i < length xs \<Longrightarrow> set (shuffles xs ! i) = set xs"
  apply (induct xs arbitrary: i)
   apply simp
  apply auto
     apply (meson in_set_dropD set_ConsD)
    apply (meson in_set_takeD set_ConsD)
   apply (metis drop_Cons' list.set_intros(1) take_Cons')
  by (metis Un_iff Un_insert_right append_take_drop_id drop0
            length_pos_if_in_set list.set(2) set_append)

lemma shuffles_lists_elems2[simp]: "i < length xs \<Longrightarrow> TODO"
  oops


fun permutations :: "'a list \<Rightarrow> ('a list) list" where
  "permutations [] = []"
| "permutations [x] = [[x]]"
| "permutations (x#xs) =
     concat (map shuffles ((map (\<lambda> xs'.  x # xs') (permutations xs))))"


lemma perm_lists_length: "i < length xs \<Longrightarrow> length (permutations xs ! i) = length xs"
  apply (induct xs arbitrary: i)
   apply simp
  oops

lemma perm_length_fac: "length xs \<noteq> 0 \<Longrightarrow> length (permutations (xs)) = fact (length (xs))"
  apply (induct xs rule: permutations.induct)
    apply simp
   apply simp
  apply clarsimp
  apply (simp add: length_concat comp_def)

  oops

(*
definition trainperms :: "(nat \<times> nat \<times> nat \<times> nat) \<Rightarrow> (nat \<times> nat \<times> nat \<times> nat) set" where
  "trainperms \<equiv> {(a,b,c,d). }" *)


definition "(ops :: (int \<Rightarrow> int \<Rightarrow> int) set) \<equiv> {(+), (-), (*), (div)}"
definition "has_solution a b c d \<equiv> \<exists>x \<in> ops. \<exists>y \<in> ops. \<exists>z \<in> ops. z (y (x a b) c) d = 10"

declare ops_def[simp]
declare has_solution_def[simp]

lemma basic: "has_solution 1 2 3 4"
  by simp+

end